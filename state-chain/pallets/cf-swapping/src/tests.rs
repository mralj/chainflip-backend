mod fill_or_kill;

use super::*;
use crate::{
	mock::{RuntimeEvent, *},
	CcmFailReason, CcmIdCounter, CcmOutputs, CcmSwap, CcmSwapOutput, CollectedRejectedFunds,
	EarnedBrokerFees, Error, Event, MaximumSwapAmount, Pallet, PendingCcms, Swap, SwapOrigin,
	SwapQueue, SwapType,
};
use cf_amm::common::{price_to_sqrt_price, PRICE_FRACTIONAL_BITS};
use cf_chains::{
	address::{to_encoded_address, AddressConverter, EncodedAddress, ForeignChainAddress},
	btc::{BitcoinNetwork, ScriptPubkey},
	dot::PolkadotAccountId,
	AnyChain, CcmChannelMetadata, CcmDepositMetadata, Ethereum,
};
use cf_primitives::{
	Asset, AssetAmount, BasisPoints, Beneficiary, ForeignChain, NetworkEnvironment,
};
use cf_test_utilities::{assert_event_sequence, assert_events_eq};
use cf_traits::{
	mocks::{
		address_converter::MockAddressConverter,
		egress_handler::{MockEgressHandler, MockEgressParameter},
		ingress_egress_fee_handler::MockIngressEgressFeeHandler,
	},
	AccountRoleRegistry, AssetConverter, CcmHandler, Chainflip, SetSafeMode, SwapDepositHandler,
};
use frame_support::{
	assert_err, assert_noop, assert_ok,
	testing_prelude::bounded_vec,
	traits::{Hooks, OriginTrait},
};
use itertools::Itertools;
use sp_arithmetic::Permill;
use sp_core::{H160, U256};
use sp_std::iter;

const GAS_BUDGET: AssetAmount = 1_000u128;

fn set_maximum_swap_amount(asset: Asset, amount: Option<AssetAmount>) {
	assert_ok!(Swapping::update_pallet_config(
		OriginTrait::root(),
		vec![PalletConfigUpdate::MaximumSwapAmount { asset, amount }]
			.try_into()
			.unwrap()
	));
}

// Returns some test data
fn generate_test_swaps() -> Vec<Swap> {
	vec![
		// asset -> USDC
		Swap::new(
			1,
			Asset::Flip,
			Asset::Usdc,
			100,
			None,
			SwapType::Swap(ForeignChainAddress::Eth([2; 20].into())),
		),
		// USDC -> asset
		Swap::new(
			2,
			Asset::Eth,
			Asset::Usdc,
			40,
			None,
			SwapType::Swap(ForeignChainAddress::Eth([9; 20].into())),
		),
		// Both assets are on the Eth chain
		Swap::new(
			3,
			Asset::Flip,
			Asset::Eth,
			500,
			None,
			SwapType::Swap(ForeignChainAddress::Eth([2; 20].into())),
		),
		// Cross chain
		Swap::new(
			4,
			Asset::Flip,
			Asset::Dot,
			600,
			None,
			SwapType::Swap(ForeignChainAddress::Dot(PolkadotAccountId::from_aliased([4; 32]))),
		),
	]
}

fn assert_failed_ccm(
	from: Asset,
	amount: AssetAmount,
	output: Asset,
	destination_address: ForeignChainAddress,
	ccm: CcmDepositMetadata,
	reason: CcmFailReason,
) {
	assert_err!(
		Swapping::on_ccm_deposit(
			from,
			amount,
			output,
			destination_address.clone(),
			ccm.clone(),
			SwapOrigin::Vault { tx_hash: Default::default() },
			None,
		),
		()
	);
	System::assert_last_event(RuntimeEvent::Swapping(Event::CcmFailed {
		reason,
		destination_address: MockAddressConverter::to_encoded_address(destination_address),
		deposit_metadata: ccm,
		origin: SwapOrigin::Vault { tx_hash: Default::default() },
	}));
}

fn insert_swaps(swaps: &[Swap]) {
	use cf_amm::common::{bounded_sqrt_price, sqrt_price_to_price};

	for (broker_id, swap) in swaps.iter().enumerate() {
		if let SwapType::Swap(destination_address) = &swap.swap_type {
			<Pallet<Test> as SwapDepositHandler>::schedule_swap_from_channel(
				ForeignChainAddress::Eth([2; 20].into()),
				Default::default(),
				swap.from,
				swap.to,
				swap.input_amount,
				destination_address.clone(),
				bounded_vec![Beneficiary { account: broker_id as u64, bps: 2 }],
				swap.refund_params.clone().map(|params| ChannelRefundParameters {
					retry_duration: params
						.refund_block
						.checked_sub(System::block_number().try_into().unwrap())
						.expect("invalid refund block"),
					refund_address: ForeignChainAddress::Eth([10; 20].into()),
					min_price: sqrt_price_to_price(bounded_sqrt_price(
						params.min_output.into(),
						swap.input_amount.into(),
					)),
				}),
				1, /* channel id */
			);
		} else {
			panic!("Unexpected swap type: {:?}", swap.swap_type);
		}
	}
}

fn generate_ccm_channel() -> CcmChannelMetadata {
	CcmChannelMetadata {
		message: vec![0x01].try_into().unwrap(),
		gas_budget: GAS_BUDGET,
		cf_parameters: Default::default(),
	}
}
fn generate_ccm_deposit() -> CcmDepositMetadata {
	CcmDepositMetadata {
		source_chain: ForeignChain::Ethereum,
		source_address: Some(ForeignChainAddress::Eth([0xcf; 20].into())),
		channel_metadata: generate_ccm_channel(),
	}
}

#[track_caller]
fn assert_swaps_queue_is_empty() {
	assert_eq!(SwapQueue::<Test>::iter_keys().count(), 0);
}

#[test]
fn request_swap_success_with_valid_parameters() {
	new_test_ext().execute_with(|| {
		assert_ok!(Swapping::request_swap_deposit_address_with_affiliates(
			RuntimeOrigin::signed(ALICE),
			Asset::Eth,
			Asset::Usdc,
			EncodedAddress::Eth(Default::default()),
			0,
			None,
			0,
			Default::default(),
			None,
		));
	});
}

#[test]
fn process_all_swaps() {
	new_test_ext().execute_with(|| {
		let swaps = generate_test_swaps();
		insert_swaps(&swaps);
		Swapping::on_finalize(System::block_number() + SWAP_DELAY_BLOCKS as u64);
		assert_swaps_queue_is_empty();
		let mut expected = swaps
			.iter()
			.cloned()
			.map(|swap| MockEgressParameter::<AnyChain>::Swap {
				asset: swap.to,
				amount: swap.input_amount,
				destination_address: if let SwapType::Swap(destination_address) = swap.swap_type {
					destination_address
				} else {
					ForeignChainAddress::Eth(Default::default())
				},
				fee: 0,
			})
			.collect::<Vec<_>>();
		expected.sort();
		let mut egresses = MockEgressHandler::<AnyChain>::get_scheduled_egresses();
		egresses.sort();
		for (input, output) in iter::zip(expected, egresses) {
			assert_eq!(input, output);
		}
	});
}

#[test]
fn expect_earned_fees_to_be_recorded() {
	new_test_ext().execute_with(|| {
		const ALICE: u64 = 2_u64;
		const BOB: u64 = 3_u64;

		<Pallet<Test> as SwapDepositHandler>::schedule_swap_from_channel(
			ForeignChainAddress::Eth([2; 20].into()),
			Default::default(),
			Asset::Flip,
			Asset::Usdc,
			100,
			ForeignChainAddress::Eth([2; 20].into()),
			bounded_vec![Beneficiary { account: ALICE, bps: 200 }],
			None,
			1,
		);
		assert_eq!(EarnedBrokerFees::<Test>::get(ALICE, cf_primitives::Asset::Flip), 2);
		<Pallet<Test> as SwapDepositHandler>::schedule_swap_from_channel(
			ForeignChainAddress::Eth([2; 20].into()),
			Default::default(),
			Asset::Flip,
			Asset::Usdc,
			100,
			ForeignChainAddress::Eth([2; 20].into()),
			bounded_vec![Beneficiary { account: ALICE, bps: 200 }],
			None,
			1,
		);
		assert_eq!(EarnedBrokerFees::<Test>::get(ALICE, cf_primitives::Asset::Flip), 4);
		<Pallet<Test> as SwapDepositHandler>::schedule_swap_from_channel(
			ForeignChainAddress::Eth([2; 20].into()),
			Default::default(),
			Asset::Eth,
			Asset::Usdc,
			100,
			ForeignChainAddress::Eth([2; 20].into()),
			bounded_vec![
				Beneficiary { account: ALICE, bps: 200 },
				Beneficiary { account: BOB, bps: 200 }
			],
			None,
			1,
		);
		assert_eq!(EarnedBrokerFees::<Test>::get(ALICE, cf_primitives::Asset::Eth), 2);
		assert_eq!(EarnedBrokerFees::<Test>::get(BOB, cf_primitives::Asset::Eth), 2);
	});
}

#[test]
#[should_panic]
fn cannot_swap_with_incorrect_destination_address_type() {
	new_test_ext().execute_with(|| {
		const ALICE: u64 = 1_u64;
		<Pallet<Test> as SwapDepositHandler>::schedule_swap_from_channel(
			ForeignChainAddress::Eth([2; 20].into()),
			Default::default(),
			Asset::Eth,
			Asset::Dot,
			10,
			ForeignChainAddress::Eth([2; 20].into()),
			bounded_vec![Beneficiary { account: ALICE, bps: 2 }],
			None,
			1,
		);

		assert_swaps_queue_is_empty();
	});
}

#[test]
fn expect_swap_id_to_be_emitted() {
	new_test_ext()
		.execute_with(|| {
			// 1. Request a deposit address -> SwapDepositAddressReady
			assert_ok!(Swapping::request_swap_deposit_address_with_affiliates(
				RuntimeOrigin::signed(ALICE),
				Asset::Eth,
				Asset::Usdc,
				EncodedAddress::Eth(Default::default()),
				0,
				None,
				0,
				Default::default(),
				None,
			));

			const AMOUNT: AssetAmount = 500;
			// 2. Schedule the swap -> SwapScheduled
			<Pallet<Test> as SwapDepositHandler>::schedule_swap_from_channel(
				ForeignChainAddress::Eth(Default::default()),
				Default::default(),
				Asset::Eth,
				Asset::Usdc,
				AMOUNT,
				ForeignChainAddress::Eth(Default::default()),
				bounded_vec![],
				None,
				1,
			);
			// 3. Process swaps -> SwapExecuted, SwapEgressScheduled
			Swapping::on_finalize(1);
			assert_event_sequence!(
				Test,
				RuntimeEvent::Swapping(Event::SwapDepositAddressReady {
					deposit_address: EncodedAddress::Eth(..),
					destination_address: EncodedAddress::Eth(..),
					source_asset: Asset::Eth,
					destination_asset: Asset::Usdc,
					channel_id: 0,
					..
				}),
				RuntimeEvent::Swapping(Event::SwapScheduled {
					swap_id: 1,
					source_asset: Asset::Eth,
					deposit_amount: AMOUNT,
					destination_asset: Asset::Usdc,
					destination_address: EncodedAddress::Eth(..),
					origin: SwapOrigin::DepositChannel {
						deposit_address: EncodedAddress::Eth(..),
						channel_id: 1,
						deposit_block_height: 0
					},
					swap_type: SwapType::Swap(ForeignChainAddress::Eth(..)),
					..
				})
			);
		})
		.then_process_blocks_until(|_| System::block_number() == 3)
		.then_execute_with(|_| {
			assert_event_sequence!(
				Test,
				RuntimeEvent::Swapping(Event::<Test>::NetworkFeeTaken {
					swap_id: 1,
					fee_amount: 0,
				}),
				RuntimeEvent::Swapping(Event::SwapExecuted { swap_id: 1, .. }),
				RuntimeEvent::Swapping(Event::SwapEgressScheduled {
					swap_id: 1,
					egress_id: (ForeignChain::Ethereum, 1),
					asset: Asset::Usdc,
					amount: 500,
					fee: _,
				})
			);
		});
}

#[test]
fn withdraw_broker_fees() {
	new_test_ext().execute_with(|| {
		assert_noop!(
			Swapping::withdraw(
				RuntimeOrigin::signed(ALICE),
				Asset::Eth,
				EncodedAddress::Eth(Default::default()),
			),
			<Error<Test>>::NoFundsAvailable
		);
		EarnedBrokerFees::<Test>::insert(ALICE, Asset::Eth, 200);
		assert_ok!(Swapping::withdraw(
			RuntimeOrigin::signed(ALICE),
			Asset::Eth,
			EncodedAddress::Eth(Default::default()),
		));
		let mut egresses = MockEgressHandler::<AnyChain>::get_scheduled_egresses();
		assert!(egresses.len() == 1);
		assert_eq!(egresses.pop().expect("must exist").amount(), 200);
		System::assert_last_event(RuntimeEvent::Swapping(Event::<Test>::WithdrawalRequested {
			egress_id: (ForeignChain::Ethereum, 1),
			egress_asset: Asset::Eth,
			egress_amount: 200,
			destination_address: EncodedAddress::Eth(Default::default()),
			egress_fee: 0,
		}));
	});
}

#[test]
fn can_swap_using_witness_origin() {
	new_test_ext().execute_with(|| {
		let from = Asset::Eth;
		let to = Asset::Flip;
		let amount = 1_000;

		assert_ok!(Swapping::schedule_swap_from_contract(
			RuntimeOrigin::root(),
			from,
			to,
			amount,
			EncodedAddress::Eth(Default::default()),
			Default::default(),
		));

		System::assert_last_event(RuntimeEvent::Swapping(Event::<Test>::SwapScheduled {
			swap_id: 1,
			source_asset: from,
			deposit_amount: amount,
			destination_asset: to,
			destination_address: EncodedAddress::Eth(Default::default()),
			origin: SwapOrigin::Vault { tx_hash: Default::default() },
			swap_type: SwapType::Swap(ForeignChainAddress::Eth(Default::default())),
			broker_commission: None,
			broker_fee: None,
			execute_at: 3,
		}));
	});
}

#[test]
fn reject_invalid_ccm_deposit() {
	new_test_ext().execute_with(|| {
		let gas_budget = GAS_BUDGET;
		let ccm = generate_ccm_deposit();

		assert_noop!(
			Swapping::ccm_deposit(
				RuntimeOrigin::root(),
				Asset::Btc,
				1_000_000,
				Asset::Eth,
				EncodedAddress::Dot(Default::default()),
				ccm.clone(),
				Default::default(),
			),
			Error::<Test>::IncompatibleAssetAndAddress
		);

		assert_noop!(
			Swapping::ccm_deposit(
				RuntimeOrigin::root(),
				Asset::Btc,
				1_000_000,
				Asset::Eth,
				EncodedAddress::Dot(Default::default()),
				ccm.clone(),
				Default::default(),
			),
			Error::<Test>::IncompatibleAssetAndAddress
		);

		assert_failed_ccm(
			Asset::Eth,
			1_000_000,
			Asset::Dot,
			ForeignChainAddress::Dot(Default::default()),
			ccm.clone(),
			CcmFailReason::UnsupportedForTargetChain,
		);

		assert_failed_ccm(
			Asset::Eth,
			1_000_000,
			Asset::Btc,
			ForeignChainAddress::Btc(cf_chains::btc::ScriptPubkey::P2PKH(Default::default())),
			ccm.clone(),
			CcmFailReason::UnsupportedForTargetChain,
		);
		assert_failed_ccm(
			Asset::Eth,
			gas_budget - 1,
			Asset::Eth,
			ForeignChainAddress::Eth(Default::default()),
			ccm,
			CcmFailReason::InsufficientDepositAmount,
		);
	});
}

#[test]
fn rejects_invalid_swap_deposit() {
	new_test_ext().execute_with(|| {
		let ccm = generate_ccm_channel();

		assert_noop!(
			Swapping::request_swap_deposit_address_with_affiliates(
				RuntimeOrigin::signed(ALICE),
				Asset::Btc,
				Asset::Eth,
				EncodedAddress::Dot(Default::default()),
				0,
				Some(ccm.clone()),
				0,
				Default::default(),
				None,
			),
			Error::<Test>::IncompatibleAssetAndAddress
		);

		assert_noop!(
			Swapping::request_swap_deposit_address_with_affiliates(
				RuntimeOrigin::signed(ALICE),
				Asset::Eth,
				Asset::Dot,
				EncodedAddress::Dot(Default::default()),
				0,
				Some(ccm),
				0,
				Default::default(),
				None,
			),
			Error::<Test>::CcmUnsupportedForTargetChain
		);
	});
}

#[test]
fn rejects_invalid_swap_by_witnesser() {
	new_test_ext().execute_with(|| {
		let script_pubkey = ScriptPubkey::try_from_address(
			"BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4",
			&BitcoinNetwork::Mainnet,
		)
		.unwrap();

		let btc_encoded_address =
			to_encoded_address(ForeignChainAddress::Btc(script_pubkey), || {
				NetworkEnvironment::Mainnet
			});

		// Is valid Bitcoin address, but asset is Dot, so not compatible
		assert_noop!(
			Swapping::schedule_swap_from_contract(
				RuntimeOrigin::root(),
				Asset::Eth,
				Asset::Dot,
				10000,
				btc_encoded_address,
				Default::default()
			),
			Error::<Test>::IncompatibleAssetAndAddress
		);

		assert_noop!(
			Swapping::schedule_swap_from_contract(
				RuntimeOrigin::root(),
				Asset::Eth,
				Asset::Btc,
				10000,
				EncodedAddress::Btc(vec![0x41, 0x80, 0x41]),
				Default::default()
			),
			Error::<Test>::InvalidDestinationAddress
		);
	});
}

#[test]
fn can_process_ccms_via_swap_deposit_address() {
	new_test_ext().execute_with(|| {
		let gas_budget = GAS_BUDGET;
		let deposit_amount = 10_000;
		let request_ccm = generate_ccm_channel();
		let ccm = generate_ccm_deposit();

		// Can process CCM via Swap deposit
		assert_ok!(Swapping::request_swap_deposit_address_with_affiliates(
			RuntimeOrigin::signed(ALICE),
			Asset::Dot,
			Asset::Eth,
			EncodedAddress::Eth(Default::default()),
			0,
			Some(request_ccm),
			0,
			Default::default(),
			None,
		));
		assert_ok!(Swapping::on_ccm_deposit(
			Asset::Dot,
			deposit_amount,
			Asset::Eth,
			ForeignChainAddress::Eth(Default::default()),
			ccm.clone(),
			SwapOrigin::Vault { tx_hash: Default::default() },
			None,
		));

		assert_eq!(
			PendingCcms::<Test>::get(1),
			Some(CcmSwap {
				source_asset: Asset::Dot,
				deposit_amount,
				destination_asset: Asset::Eth,
				destination_address: ForeignChainAddress::Eth(Default::default()),
				deposit_metadata: ccm,
				principal_swap_id: Some(1),
				gas_swap_id: Some(2),
			})
		);

		let execute_at = System::block_number() + u64::from(SWAP_DELAY_BLOCKS);

		assert_eq!(
			SwapQueue::<Test>::get(execute_at),
			vec![
				Swap::new(
					1,
					Asset::Dot,
					Asset::Eth,
					deposit_amount - gas_budget,
					None,
					SwapType::CcmPrincipal(1),
				),
				Swap::new(2, Asset::Dot, Asset::Eth, gas_budget, None, SwapType::CcmGas(1)),
			]
		);

		assert_eq!(CcmOutputs::<Test>::get(1), Some(CcmSwapOutput { principal: None, gas: None }));

		// Swaps are executed during on_finalize after SWAP_DELAY_BLOCKS delay
		Swapping::on_finalize(execute_at);

		// CCM is scheduled for egress
		assert_eq!(
			MockEgressHandler::<AnyChain>::get_scheduled_egresses(),
			vec![MockEgressParameter::Ccm {
				asset: Asset::Eth,
				amount: deposit_amount - gas_budget,
				destination_address: ForeignChainAddress::Eth(Default::default()),
				message: vec![0x01].try_into().unwrap(),
				cf_parameters: vec![].try_into().unwrap(),
				gas_budget,
			},]
		);

		// Completed CCM is removed from storage
		assert_eq!(PendingCcms::<Test>::get(1), None);
		assert_eq!(CcmOutputs::<Test>::get(1), None);

		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::CcmEgressScheduled {
			ccm_id: CcmIdCounter::<Test>::get(),
			egress_id: (ForeignChain::Ethereum, 1),
		}));
	});
}

#[test]
fn can_process_ccms_via_extrinsic() {
	new_test_ext().execute_with(|| {
		let gas_budget = GAS_BUDGET;
		let deposit_amount = 1_000_000;
		let ccm = generate_ccm_deposit();

		// Can process CCM directly via Pallet Extrinsic.
		assert_ok!(Swapping::ccm_deposit(
			RuntimeOrigin::root(),
			Asset::Btc,
			deposit_amount,
			Asset::Usdc,
			EncodedAddress::Eth(Default::default()),
			ccm.clone(),
			Default::default(),
		));

		assert_eq!(
			PendingCcms::<Test>::get(1),
			Some(CcmSwap {
				source_asset: Asset::Btc,
				deposit_amount,
				destination_asset: Asset::Usdc,
				destination_address: ForeignChainAddress::Eth(Default::default()),
				deposit_metadata: ccm.clone(),
				principal_swap_id: Some(1),
				gas_swap_id: Some(2),
			})
		);

		let execute_at = System::block_number() + u64::from(SWAP_DELAY_BLOCKS);

		assert_eq!(
			SwapQueue::<Test>::get(execute_at),
			vec![
				Swap::new(
					1,
					Asset::Btc,
					Asset::Usdc,
					deposit_amount - gas_budget,
					None,
					SwapType::CcmPrincipal(1),
				),
				Swap::new(2, Asset::Btc, Asset::Eth, gas_budget, None, SwapType::CcmGas(1))
			]
		);
		assert_eq!(CcmOutputs::<Test>::get(1), Some(CcmSwapOutput { principal: None, gas: None }));

		// Swaps are executed during on_finalize
		Swapping::on_finalize(execute_at);

		// CCM is scheduled for egress
		assert_eq!(
			MockEgressHandler::<AnyChain>::get_scheduled_egresses(),
			vec![MockEgressParameter::Ccm {
				asset: Asset::Usdc,
				amount: deposit_amount - gas_budget,
				destination_address: ForeignChainAddress::Eth(Default::default()),
				message: vec![0x01].try_into().unwrap(),
				cf_parameters: vec![].try_into().unwrap(),
				gas_budget,
			},]
		);

		// Completed CCM is removed from storage
		assert_eq!(PendingCcms::<Test>::get(1), None);
		assert_eq!(CcmOutputs::<Test>::get(1), None);

		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::CcmDepositReceived {
			ccm_id: CcmIdCounter::<Test>::get(),
			principal_swap_id: Some(1),
			gas_swap_id: Some(2),
			deposit_amount,
			destination_address: EncodedAddress::Eth(Default::default()),
			deposit_metadata: ccm,
		}));
		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::CcmEgressScheduled {
			ccm_id: CcmIdCounter::<Test>::get(),
			egress_id: (ForeignChain::Ethereum, 1),
		}));
	});
}

#[test]
fn can_handle_ccms_with_non_native_gas_asset() {
	new_test_ext().execute_with(|| {
		let gas_budget = GAS_BUDGET;
		let deposit_amount = 10_000;
		let ccm = generate_ccm_deposit();
		assert_ok!(Swapping::ccm_deposit(
			RuntimeOrigin::root(),
			Asset::Eth,
			deposit_amount,
			Asset::Usdc,
			EncodedAddress::Eth(Default::default()),
			ccm.clone(),
			Default::default(),
		));

		assert_eq!(
			PendingCcms::<Test>::get(1),
			Some(CcmSwap {
				source_asset: Asset::Eth,
				deposit_amount,
				destination_asset: Asset::Usdc,
				destination_address: ForeignChainAddress::Eth(Default::default()),
				deposit_metadata: ccm.clone(),
				principal_swap_id: Some(1),
				gas_swap_id: None,
			})
		);

		let execute_at = System::block_number() + u64::from(SWAP_DELAY_BLOCKS);

		assert_eq!(
			SwapQueue::<Test>::get(execute_at),
			vec![Swap::new(
				1,
				Asset::Eth,
				Asset::Usdc,
				deposit_amount - gas_budget,
				None,
				SwapType::CcmPrincipal(1),
			)]
		);
		assert_eq!(
			CcmOutputs::<Test>::get(1),
			Some(CcmSwapOutput { principal: None, gas: Some(gas_budget) })
		);

		// Swaps are executed during on_finalize
		Swapping::on_finalize(execute_at);

		// CCM is scheduled for egress
		assert_eq!(
			MockEgressHandler::<AnyChain>::get_scheduled_egresses(),
			vec![MockEgressParameter::Ccm {
				asset: Asset::Usdc,
				amount: deposit_amount - gas_budget,
				destination_address: ForeignChainAddress::Eth(Default::default()),
				message: vec![0x01].try_into().unwrap(),
				cf_parameters: vec![].try_into().unwrap(),
				gas_budget,
			},]
		);

		// Completed CCM is removed from storage
		assert_eq!(PendingCcms::<Test>::get(1), None);
		assert_eq!(CcmOutputs::<Test>::get(1), None);

		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::CcmDepositReceived {
			ccm_id: CcmIdCounter::<Test>::get(),
			principal_swap_id: Some(1),
			gas_swap_id: None,
			deposit_amount,
			destination_address: EncodedAddress::Eth(Default::default()),
			deposit_metadata: ccm,
		}));
		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::CcmEgressScheduled {
			ccm_id: CcmIdCounter::<Test>::get(),
			egress_id: (ForeignChain::Ethereum, 1),
		}));
	});
}

#[test]
fn can_handle_ccms_with_native_gas_asset() {
	new_test_ext().execute_with(|| {
		let gas_budget = GAS_BUDGET;
		let deposit_amount = 10_000;
		let ccm = generate_ccm_deposit();

		assert_ok!(Swapping::ccm_deposit(
			RuntimeOrigin::root(),
			Asset::Usdc,
			deposit_amount,
			Asset::Usdc,
			EncodedAddress::Eth(Default::default()),
			ccm.clone(),
			Default::default(),
		));

		assert_eq!(
			PendingCcms::<Test>::get(1),
			Some(CcmSwap {
				source_asset: Asset::Usdc,
				deposit_amount,
				destination_asset: Asset::Usdc,
				destination_address: ForeignChainAddress::Eth(Default::default()),
				deposit_metadata: ccm.clone(),
				principal_swap_id: None,
				gas_swap_id: Some(1),
			})
		);

		let execute_at = System::block_number() + u64::from(SWAP_DELAY_BLOCKS);
		assert_eq!(
			SwapQueue::<Test>::get(execute_at),
			vec![Swap::new(1, Asset::Usdc, Asset::Eth, gas_budget, None, SwapType::CcmGas(1),)]
		);
		assert_eq!(
			CcmOutputs::<Test>::get(1),
			Some(CcmSwapOutput { principal: Some(deposit_amount - gas_budget), gas: None })
		);

		// Swaps are executed during on_finalize
		Swapping::on_finalize(execute_at);

		// CCM is scheduled for egress
		assert_eq!(
			MockEgressHandler::<AnyChain>::get_scheduled_egresses(),
			vec![MockEgressParameter::Ccm {
				asset: Asset::Usdc,
				amount: deposit_amount - gas_budget,
				destination_address: ForeignChainAddress::Eth(Default::default()),
				message: vec![0x01].try_into().unwrap(),
				cf_parameters: vec![].try_into().unwrap(),
				gas_budget,
			},]
		);

		// Completed CCM is removed from storage
		assert_eq!(PendingCcms::<Test>::get(1), None);
		assert_eq!(CcmOutputs::<Test>::get(1), None);

		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::CcmDepositReceived {
			ccm_id: CcmIdCounter::<Test>::get(),
			principal_swap_id: None,
			gas_swap_id: Some(1),
			deposit_amount,
			destination_address: EncodedAddress::Eth(Default::default()),
			deposit_metadata: ccm,
		}));
		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::CcmEgressScheduled {
			ccm_id: CcmIdCounter::<Test>::get(),
			egress_id: (ForeignChain::Ethereum, 1),
		}));
	});
}

#[test]
fn can_handle_ccms_with_no_swaps_needed() {
	new_test_ext().execute_with(|| {
		let gas_budget = GAS_BUDGET;
		let deposit_amount = 10_000;
		let ccm = generate_ccm_deposit();

		// Ccm without need for swapping are egressed directly.
		assert_ok!(Swapping::ccm_deposit(
			RuntimeOrigin::root(),
			Asset::Eth,
			deposit_amount,
			Asset::Eth,
			EncodedAddress::Eth(Default::default()),
			ccm.clone(),
			Default::default(),
		));

		assert_eq!(PendingCcms::<Test>::get(1), None);

		// The ccm is never put in storage
		assert_eq!(PendingCcms::<Test>::get(1), None);
		assert_eq!(CcmOutputs::<Test>::get(1), None);

		// CCM is scheduled for egress
		assert_eq!(
			MockEgressHandler::<AnyChain>::get_scheduled_egresses(),
			vec![MockEgressParameter::Ccm {
				asset: Asset::Eth,
				amount: deposit_amount - gas_budget,
				destination_address: ForeignChainAddress::Eth(Default::default()),
				message: vec![0x01].try_into().unwrap(),
				cf_parameters: vec![].try_into().unwrap(),
				gas_budget,
			},]
		);

		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::CcmEgressScheduled {
			ccm_id: CcmIdCounter::<Test>::get(),
			egress_id: (ForeignChain::Ethereum, 1),
		}));

		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::CcmDepositReceived {
			ccm_id: CcmIdCounter::<Test>::get(),
			principal_swap_id: None,
			gas_swap_id: None,
			deposit_amount,
			destination_address: EncodedAddress::Eth(Default::default()),
			deposit_metadata: ccm,
		}));
	});
}

#[test]
fn swap_by_witnesser_happy_path() {
	new_test_ext().execute_with(|| {
		let from = Asset::Eth;
		let to = Asset::Flip;
		let amount = 1_000u128;

		assert_ok!(Swapping::schedule_swap_from_contract(
			RuntimeOrigin::root(),
			from,
			to,
			amount,
			EncodedAddress::Eth(Default::default()),
			Default::default(),
		));

		let execute_at = System::block_number() + u64::from(SWAP_DELAY_BLOCKS);

		// Verify this swap is accepted and scheduled
		assert_eq!(
			SwapQueue::<Test>::get(execute_at),
			vec![Swap::new(
				1,
				from,
				to,
				amount,
				None,
				SwapType::Swap(ForeignChainAddress::Eth(Default::default()),),
			)]
		);
		System::assert_last_event(RuntimeEvent::Swapping(Event::<Test>::SwapScheduled {
			swap_id: 1,
			source_asset: from,
			deposit_amount: amount,
			destination_asset: to,
			destination_address: EncodedAddress::Eth(Default::default()),
			origin: SwapOrigin::Vault { tx_hash: Default::default() },
			swap_type: SwapType::Swap(ForeignChainAddress::Eth(Default::default())),
			broker_commission: None,
			broker_fee: None,
			execute_at,
		}));

		// Confiscated fund is unchanged
		assert_eq!(CollectedRejectedFunds::<Test>::get(from), 0);
	});
}

#[test]
fn swap_by_deposit_happy_path() {
	new_test_ext().execute_with(|| {
		let from = Asset::Eth;
		let to = Asset::Flip;
		let amount = 1_000u128;

		Swapping::schedule_swap_from_channel(
			ForeignChainAddress::Eth(Default::default()),
			Default::default(),
			from,
			to,
			amount,
			ForeignChainAddress::Eth(Default::default()),
			bounded_vec![],
			None,
			1,
		);

		let execute_at = System::block_number() + u64::from(SWAP_DELAY_BLOCKS);

		// Verify this swap is accepted and scheduled
		assert_eq!(
			SwapQueue::<Test>::get(execute_at),
			vec![Swap::new(
				1,
				from,
				to,
				amount,
				None,
				SwapType::Swap(ForeignChainAddress::Eth(Default::default())),
			)]
		);
		System::assert_last_event(RuntimeEvent::Swapping(Event::<Test>::SwapScheduled {
			swap_id: 1,
			deposit_amount: amount,
			source_asset: from,
			destination_asset: to,
			destination_address: EncodedAddress::Eth(Default::default()),
			origin: SwapOrigin::DepositChannel {
				deposit_address: EncodedAddress::Eth(Default::default()),
				channel_id: 1,
				deposit_block_height: Default::default(),
			},
			swap_type: SwapType::Swap(ForeignChainAddress::Eth(Default::default())),
			broker_commission: Some(0),
			broker_fee: Some(0),
			execute_at,
		}));

		// Confiscated fund is unchanged
		assert_eq!(CollectedRejectedFunds::<Test>::get(from), 0);
	});
}

#[test]
fn ccm_without_principal_swaps_are_accepted() {
	new_test_ext().execute_with(|| {
		let gas_budget = GAS_BUDGET;
		let principal_amount = 10_000;
		let eth: Asset = Asset::Eth;
		let flip: Asset = Asset::Flip;
		let ccm = generate_ccm_deposit();

		// Ccm with principal asset = 0
		assert_ok!(Swapping::on_ccm_deposit(
			eth,
			gas_budget,
			flip,
			ForeignChainAddress::Eth(Default::default()),
			ccm.clone(),
			SwapOrigin::Vault { tx_hash: Default::default() },
			None,
		));

		// Verify the CCM is processed successfully
		assert_event_sequence!(
			Test,
			RuntimeEvent::Swapping(Event::CcmDepositReceived {
				ccm_id: 1,
				principal_swap_id: None,
				gas_swap_id: None,
				deposit_amount,
				destination_address: EncodedAddress::Eth(..),
				deposit_metadata: _,
			}) if deposit_amount == gas_budget,
			RuntimeEvent::Swapping(Event::CcmEgressScheduled {
				ccm_id: 1,
				egress_id: (ForeignChain::Ethereum, 1),
			})
		);
		// No funds are confiscated
		assert_eq!(CollectedRejectedFunds::<Test>::get(eth), 0);
		assert_eq!(CollectedRejectedFunds::<Test>::get(flip), 0);

		// Ccm where principal asset = output asset
		System::reset_events();
		assert_ok!(Swapping::on_ccm_deposit(
			eth,
			gas_budget + principal_amount,
			eth,
			ForeignChainAddress::Eth(Default::default()),
			ccm,
			SwapOrigin::Vault { tx_hash: Default::default() },
			None,
		));

		// Verify the CCM is processed successfully
		assert_event_sequence!(
			Test,
			RuntimeEvent::Swapping(Event::CcmDepositReceived {
				ccm_id: 2,
				principal_swap_id: None,
				gas_swap_id: None,
				deposit_amount,
				destination_address: EncodedAddress::Eth(..),
				deposit_metadata: _,
			}) if deposit_amount == gas_budget + principal_amount,
			RuntimeEvent::Swapping(Event::CcmEgressScheduled {
				ccm_id: 2,
				egress_id: (ForeignChain::Ethereum, 2),
			})
		);
		// No funds are confiscated
		assert_eq!(CollectedRejectedFunds::<Test>::get(eth), 0);
		assert_eq!(CollectedRejectedFunds::<Test>::get(flip), 0);
	});
}

#[test]
fn process_all_into_stable_swaps_first() {
	new_test_ext().execute_with(|| {
		let amount = 1_000_000;
		let encoded_address = EncodedAddress::Eth(Default::default());
		let address = ForeignChainAddress::Eth(Default::default());
		assert_ok!(Swapping::schedule_swap_from_contract(
			RuntimeOrigin::root(),
			Asset::Flip,
			Asset::Eth,
			amount,
			encoded_address.clone(),
			Default::default(),
		));
		assert_ok!(Swapping::schedule_swap_from_contract(
			RuntimeOrigin::root(),
			Asset::Btc,
			Asset::Eth,
			amount,
			encoded_address.clone(),
			Default::default(),
		));
		assert_ok!(Swapping::schedule_swap_from_contract(
			RuntimeOrigin::root(),
			Asset::Dot,
			Asset::Eth,
			amount,
			encoded_address.clone(),
			Default::default(),
		));
		assert_ok!(Swapping::schedule_swap_from_contract(
			RuntimeOrigin::root(),
			Asset::Usdc,
			Asset::Eth,
			amount,
			encoded_address,
			Default::default(),
		));

		let execute_at = System::block_number() + u64::from(SWAP_DELAY_BLOCKS);

		assert_eq!(
			SwapQueue::<Test>::get(execute_at),
			vec![
				Swap::new(
					1,
					Asset::Flip,
					Asset::Eth,
					amount,
					None,
					SwapType::Swap(address.clone()),
				),
				Swap::new(2, Asset::Btc, Asset::Eth, amount, None, SwapType::Swap(address.clone()),),
				Swap::new(3, Asset::Dot, Asset::Eth, amount, None, SwapType::Swap(address.clone()),),
				Swap::new(4, Asset::Usdc, Asset::Eth, amount, None, SwapType::Swap(address)),
			]
		);

		System::reset_events();
		// All swaps in the SwapQueue are executed.
		Swapping::on_finalize(execute_at);
		assert_swaps_queue_is_empty();

		// Network fee should only be taken once.
		let total_amount_after_network_fee =
			Swapping::take_network_fee(amount * 4).remaining_amount;
		let output_amount = total_amount_after_network_fee / 4;
		// Verify swap "from" -> STABLE_ASSET, then "to" -> Output Asset
		assert_eq!(
			Swaps::get(),
			vec![
				(Asset::Flip, Asset::Usdc, amount),
				(Asset::Dot, Asset::Usdc, amount),
				(Asset::Btc, Asset::Usdc, amount),
				(Asset::Usdc, Asset::Eth, total_amount_after_network_fee),
			]
		);

		assert_event_sequence!(
			Test,
			RuntimeEvent::Swapping(Event::<Test>::NetworkFeeTaken { swap_id: 1, .. }),
			RuntimeEvent::Swapping(Event::<Test>::NetworkFeeTaken { swap_id: 2, .. }),
			RuntimeEvent::Swapping(Event::<Test>::NetworkFeeTaken { swap_id: 3, .. }),
			RuntimeEvent::Swapping(Event::<Test>::NetworkFeeTaken { swap_id: 4, .. }),
			RuntimeEvent::Swapping(Event::SwapExecuted { swap_id: 1, .. }),
			RuntimeEvent::Swapping(Event::SwapEgressScheduled {
				swap_id: 1,
				asset: Asset::Eth,
				egress_id: (ForeignChain::Ethereum, 1),
				amount,
				fee: _,
			}) if amount == output_amount,
			RuntimeEvent::Swapping(Event::SwapExecuted { swap_id: 2, .. }),
			RuntimeEvent::Swapping(Event::SwapEgressScheduled {
				swap_id: 2,
				asset: Asset::Eth,
				egress_id: (ForeignChain::Ethereum, 2),
				amount,
				fee: _,
			}) if amount == output_amount,
			RuntimeEvent::Swapping(Event::SwapExecuted { swap_id: 3, .. }),
			RuntimeEvent::Swapping(Event::SwapEgressScheduled {
				swap_id: 3,
				asset: Asset::Eth,
				egress_id: (ForeignChain::Ethereum, 3),
				amount,
				fee: _,
			}) if amount == output_amount,
			RuntimeEvent::Swapping(Event::SwapExecuted { swap_id: 4, .. }),
			RuntimeEvent::Swapping(Event::SwapEgressScheduled {
				swap_id: 4,
				asset: Asset::Eth,
				egress_id: (ForeignChain::Ethereum, 4),
				amount,
				fee: _,
			}) if amount == output_amount,
		);
	});
}

#[test]
fn cannot_swap_in_safe_mode() {
	new_test_ext().execute_with(|| {
		let swaps_scheduled_at = System::block_number() + SWAP_DELAY_BLOCKS as u64;

		SwapQueue::<Test>::insert(swaps_scheduled_at, generate_test_swaps());

		assert_eq!(SwapQueue::<Test>::decode_len(swaps_scheduled_at), Some(4));

		// Activate code red
		<MockRuntimeSafeMode as SetSafeMode<MockRuntimeSafeMode>>::set_code_red();

		// No swap is done
		Swapping::on_finalize(swaps_scheduled_at);

		let retry_at_block = swaps_scheduled_at + SwapRetryDelay::<Test>::get();
		assert_eq!(SwapQueue::<Test>::decode_len(retry_at_block), Some(4));

		<MockRuntimeSafeMode as SetSafeMode<MockRuntimeSafeMode>>::set_code_green();

		// Swaps are processed
		Swapping::on_finalize(retry_at_block);
		assert_eq!(SwapQueue::<Test>::decode_len(retry_at_block), None);
	});
}

#[test]
fn cannot_withdraw_in_safe_mode() {
	new_test_ext().execute_with(|| {
		EarnedBrokerFees::<Test>::insert(ALICE, Asset::Eth, 200);

		// Activate code red
		<MockRuntimeSafeMode as SetSafeMode<MockRuntimeSafeMode>>::set_code_red();

		// Cannot withdraw
		assert_noop!(
			Swapping::withdraw(
				RuntimeOrigin::signed(ALICE),
				Asset::Eth,
				EncodedAddress::Eth(Default::default()),
			),
			Error::<Test>::WithdrawalsDisabled
		);
		assert_eq!(EarnedBrokerFees::<Test>::get(ALICE, Asset::Eth), 200);

		// Change back to code green
		<MockRuntimeSafeMode as SetSafeMode<MockRuntimeSafeMode>>::set_code_green();

		// withdraws are now alloed
		assert_ok!(Swapping::withdraw(
			RuntimeOrigin::signed(ALICE),
			Asset::Eth,
			EncodedAddress::Eth(Default::default()),
		));
		assert_eq!(EarnedBrokerFees::<Test>::get(ALICE, Asset::Eth), 0);
	});
}

#[test]
fn ccm_swaps_emits_events() {
	new_test_ext().execute_with(|| {
		let ccm = generate_ccm_deposit();
		let destination_address = ForeignChainAddress::Eth(Default::default());

		const ORIGIN: SwapOrigin = SwapOrigin::Vault { tx_hash: [0x11; 32] };

		// Test when both principal and gas need to be swapped.
		System::reset_events();
		assert_ok!(Swapping::on_ccm_deposit(
			Asset::Flip,
			10_000,
			Asset::Usdc,
			destination_address.clone(),
			ccm.clone(),
			ORIGIN,
			None,
		));
		assert_event_sequence!(
			Test,
			RuntimeEvent::Swapping(Event::SwapScheduled {
				swap_id: 1,
				source_asset: Asset::Flip,
				deposit_amount: 9_000,
				destination_asset: Asset::Usdc,
				destination_address: EncodedAddress::Eth(..),
				origin: ORIGIN,
				swap_type: SwapType::CcmPrincipal(1),
				..
			}),
			RuntimeEvent::Swapping(Event::SwapScheduled {
				swap_type: SwapType::CcmGas(1),
				swap_id: 2,
				source_asset: Asset::Flip,
				deposit_amount: 1_000,
				destination_asset: Asset::Eth,
				destination_address: EncodedAddress::Eth(..),
				origin: ORIGIN,
				..
			}),
			RuntimeEvent::Swapping(Event::CcmDepositReceived {
				ccm_id: 1,
				principal_swap_id: Some(1),
				gas_swap_id: Some(2),
				deposit_amount: 10_000,
				..
			}),
		);

		// Test when only principal needs to be swapped.
		System::reset_events();
		assert_ok!(Swapping::on_ccm_deposit(
			Asset::Eth,
			10_000,
			Asset::Usdc,
			destination_address.clone(),
			ccm.clone(),
			ORIGIN,
			None,
		));
		assert_event_sequence!(
			Test,
			RuntimeEvent::Swapping(Event::SwapScheduled {
				swap_type: SwapType::CcmPrincipal(2),
				swap_id: 3,
				source_asset: Asset::Eth,
				deposit_amount: 9_000,
				destination_asset: Asset::Usdc,
				destination_address: EncodedAddress::Eth(..),
				origin: ORIGIN,
				..
			}),
			RuntimeEvent::Swapping(Event::CcmDepositReceived {
				ccm_id: 2,
				principal_swap_id: Some(3),
				gas_swap_id: None,
				deposit_amount: 10_000,
				..
			}),
		);

		// Test when only gas needs to be swapped.
		System::reset_events();
		assert_ok!(Swapping::on_ccm_deposit(
			Asset::Flip,
			10_000,
			Asset::Flip,
			destination_address,
			ccm,
			ORIGIN,
			None,
		));
		assert_event_sequence!(
			Test,
			RuntimeEvent::Swapping(Event::SwapScheduled {
				swap_type: SwapType::CcmGas(3),
				swap_id: 4,
				source_asset: Asset::Flip,
				deposit_amount: 1_000,
				destination_asset: Asset::Eth,
				destination_address: EncodedAddress::Eth(..),
				origin: ORIGIN,
				..
			}),
			RuntimeEvent::Swapping(Event::CcmDepositReceived {
				ccm_id: 3,
				principal_swap_id: None,
				gas_swap_id: Some(4),
				deposit_amount: 10_000,
				..
			}),
		);
	});
}

#[allow(deprecated)]
#[test]
fn can_handle_ccm_with_zero_swap_outputs() {
	new_test_ext()
		.then_execute_at_next_block(|_| {
			let eth_address = ForeignChainAddress::Eth(Default::default());
			let ccm = generate_ccm_deposit();

			assert_ok!(Swapping::on_ccm_deposit(
				Asset::Usdc,
				100_000,
				Asset::Eth,
				eth_address,
				ccm,
				SwapOrigin::Vault { tx_hash: Default::default() },
				None,
			));

			// Change the swap rate so swap output will be 0
			SwapRate::set(0.0001f64);
			System::reset_events();
		})
		.then_process_blocks_until(|_| System::block_number() == 4)
		.then_execute_with(|_| {
			// Swap outputs are zero
			assert_event_sequence!(
				Test,
				RuntimeEvent::Swapping(Event::<Test>::NetworkFeeTaken { swap_id: 1, .. }),
				RuntimeEvent::Swapping(Event::<Test>::NetworkFeeTaken { swap_id: 2, .. }),
				RuntimeEvent::Swapping(Event::<Test>::SwapExecuted {
					swap_id: 1,
					source_asset: Asset::Usdc,
					destination_asset: Asset::Eth,
					deposit_amount: 99_000,
					egress_amount: 9,
					swap_input: 99_000,
					swap_output: 9,
					intermediate_amount: None,
					swap_type: SwapType::CcmPrincipal(1),
				}),
				RuntimeEvent::Swapping(Event::<Test>::SwapExecuted {
					swap_id: 2,
					source_asset: Asset::Usdc,
					deposit_amount: 1_000,
					destination_asset: Asset::Eth,
					egress_amount: 0,
					swap_input: 1_000,
					swap_output: 0,
					intermediate_amount: None,
					swap_type: SwapType::CcmGas(1),
				}),
			);

			// CCM are processed and egressed even if principal output is zero.
			assert_eq!(MockEgressHandler::<AnyChain>::get_scheduled_egresses().len(), 1);
			assert_swaps_queue_is_empty();
		});
}

#[test]
fn can_handle_swaps_with_zero_outputs() {
	new_test_ext()
		.then_execute_at_next_block(|_| {
			let eth_address = ForeignChainAddress::Eth(Default::default());

			Swapping::schedule_swap_from_channel(
				eth_address.clone(),
				Default::default(),
				Asset::Usdc,
				Asset::Eth,
				100,
				eth_address.clone(),
				bounded_vec![],
				None,
				0,
			);
			Swapping::schedule_swap_from_channel(
				eth_address.clone(),
				Default::default(),
				Asset::Usdc,
				Asset::Eth,
				1,
				eth_address,
				bounded_vec![],
				None,
				0,
			);

			// Change the swap rate so swap output will be 0
			SwapRate::set(0.01f64);
			System::reset_events();
		})
		.then_process_blocks_until(|_| System::block_number() == 4)
		.then_execute_with(|_| {
			// Swap outputs are zero
			assert_event_sequence!(
				Test,
				RuntimeEvent::Swapping(Event::<Test>::NetworkFeeTaken { swap_id: 1, .. }),
				RuntimeEvent::Swapping(Event::<Test>::NetworkFeeTaken { swap_id: 2, .. }),
				RuntimeEvent::Swapping(Event::<Test>::SwapExecuted {
					swap_id: 1,
					destination_asset: Asset::Eth,
					swap_output: 0,
					..
				}),
				RuntimeEvent::Swapping(Event::SwapEgressIgnored { swap_id: 1, .. }),
				RuntimeEvent::Swapping(Event::<Test>::SwapExecuted {
					swap_id: 2,
					destination_asset: Asset::Eth,
					swap_output: 0,
					..
				}),
				RuntimeEvent::Swapping(Event::SwapEgressIgnored { swap_id: 2, .. }),
			);

			// Swaps are not egressed when output is 0.
			assert_swaps_queue_is_empty();
			assert!(
				MockEgressHandler::<AnyChain>::get_scheduled_egresses().is_empty(),
				"No egresses should be scheduled."
			);
		});
}

#[test]
fn can_set_maximum_swap_amount() {
	new_test_ext().execute_with(|| {
		let asset = Asset::Eth;
		let amount = Some(1_000u128);
		assert!(MaximumSwapAmount::<Test>::get(asset).is_none());

		// Set the new maximum swap_amount
		set_maximum_swap_amount(asset, amount);

		assert_eq!(MaximumSwapAmount::<Test>::get(asset), amount);
		assert_eq!(Swapping::maximum_swap_amount(asset), amount);

		System::assert_last_event(RuntimeEvent::Swapping(Event::<Test>::MaximumSwapAmountSet {
			asset,
			amount,
		}));

		// Can remove maximum swap amount
		set_maximum_swap_amount(asset, None);
		assert!(MaximumSwapAmount::<Test>::get(asset).is_none());
		System::assert_last_event(RuntimeEvent::Swapping(Event::<Test>::MaximumSwapAmountSet {
			asset,
			amount: None,
		}));
	});
}

#[test]
fn swap_excess_are_confiscated_ccm_via_deposit() {
	new_test_ext().execute_with(|| {
		let gas_budget = GAS_BUDGET;
		let principal_amount = 1_000;
		let max_swap = 100;
		let from: Asset = Asset::Usdc;
		let to: Asset = Asset::Flip;
		let request_ccm = generate_ccm_channel();
		let ccm = generate_ccm_deposit();

		set_maximum_swap_amount(from, Some(max_swap));

		// Register CCM via Swap deposit
		assert_ok!(Swapping::request_swap_deposit_address_with_affiliates(
			RuntimeOrigin::signed(ALICE),
			from,
			to,
			EncodedAddress::Eth(Default::default()),
			0,
			Some(request_ccm),
			0,
			Default::default(),
			None
		));

		assert_ok!(Swapping::on_ccm_deposit(
			from,
			gas_budget + principal_amount,
			to,
			ForeignChainAddress::Eth(Default::default()),
			ccm.clone(),
			SwapOrigin::Vault { tx_hash: Default::default() },
			None,
		));

		// Excess fee is confiscated
		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::SwapAmountConfiscated {
			swap_id: 1,
			source_asset: from,
			destination_asset: to,
			total_amount: 1_000,
			confiscated_amount: 900,
		}));

		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::SwapAmountConfiscated {
			swap_id: 2,
			source_asset: from,
			destination_asset: Asset::Eth,
			total_amount: 1_000,
			confiscated_amount: 900,
		}));

		let execute_at = System::block_number() + u64::from(SWAP_DELAY_BLOCKS);
		assert_eq!(
			SwapQueue::<Test>::get(execute_at),
			vec![
				Swap::new(1, from, to, max_swap, None, SwapType::CcmPrincipal(1)),
				Swap::new(2, from, Asset::Eth, max_swap, None, SwapType::CcmGas(1)),
			]
		);
		assert_eq!(CollectedRejectedFunds::<Test>::get(from), 900 * 2);
	});
}

#[test]
fn swap_excess_are_confiscated_ccm_via_extrinsic() {
	new_test_ext().execute_with(|| {
		let gas_budget = GAS_BUDGET;
		let principal_amount = 1_000;
		let max_swap = 100;
		let from: Asset = Asset::Usdc;
		let to: Asset = Asset::Flip;
		let ccm = generate_ccm_deposit();

		set_maximum_swap_amount(from, Some(max_swap));

		// Register CCM via Swap deposit
		assert_ok!(Swapping::ccm_deposit(
			RuntimeOrigin::root(),
			from,
			gas_budget + principal_amount,
			to,
			EncodedAddress::Eth(Default::default()),
			ccm,
			Default::default(),
		));

		// Excess fee is confiscated
		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::SwapAmountConfiscated {
			swap_id: 1,
			source_asset: from,
			destination_asset: to,
			total_amount: 1_000,
			confiscated_amount: 900,
		}));

		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::SwapAmountConfiscated {
			swap_id: 2,
			source_asset: from,
			destination_asset: Asset::Eth,
			total_amount: 1_000,
			confiscated_amount: 900,
		}));

		let execute_at = System::block_number() + u64::from(SWAP_DELAY_BLOCKS);
		assert_eq!(
			SwapQueue::<Test>::get(execute_at),
			vec![
				Swap::new(1, from, to, max_swap, None, SwapType::CcmPrincipal(1)),
				Swap::new(2, from, Asset::Eth, max_swap, None, SwapType::CcmGas(1)),
			]
		);
		assert_eq!(CollectedRejectedFunds::<Test>::get(from), 900 * 2);
	});
}

#[test]
fn swap_excess_are_confiscated_for_swap_via_extrinsic() {
	new_test_ext().execute_with(|| {
		let max_swap = 100;
		let amount = 1_000;
		let from: Asset = Asset::Usdc;
		let to: Asset = Asset::Flip;

		set_maximum_swap_amount(from, Some(max_swap));

		assert_ok!(Swapping::schedule_swap_from_contract(
			RuntimeOrigin::signed(ALICE),
			from,
			to,
			amount,
			EncodedAddress::Eth(Default::default()),
			Default::default(),
		));

		// Excess fee is confiscated
		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::SwapAmountConfiscated {
			swap_id: 1,
			source_asset: from,
			destination_asset: to,
			total_amount: 1_000,
			confiscated_amount: 900,
		}));

		assert_eq!(
			SwapQueue::<Test>::get(System::block_number() + u64::from(SWAP_DELAY_BLOCKS)),
			vec![Swap::new(
				1,
				from,
				to,
				max_swap,
				None,
				SwapType::Swap(ForeignChainAddress::Eth(Default::default()))
			)]
		);
		assert_eq!(CollectedRejectedFunds::<Test>::get(from), 900);
	});
}

#[test]
fn swap_excess_are_confiscated_for_swap_via_deposit() {
	new_test_ext().execute_with(|| {
		let max_swap = 100;
		let amount = 1_000;
		let from: Asset = Asset::Usdc;
		let to: Asset = Asset::Flip;

		set_maximum_swap_amount(from, Some(max_swap));

		Swapping::schedule_swap_from_channel(
			ForeignChainAddress::Eth(Default::default()),
			1,
			from,
			to,
			amount,
			ForeignChainAddress::Eth(Default::default()),
			bounded_vec![],
			None,
			0,
		);

		// Excess fee is confiscated
		System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::SwapAmountConfiscated {
			swap_id: 1,
			source_asset: from,
			destination_asset: to,
			total_amount: 1_000,
			confiscated_amount: 900,
		}));

		assert_eq!(
			SwapQueue::<Test>::get(System::block_number() + u64::from(SWAP_DELAY_BLOCKS)),
			vec![Swap::new(
				1,
				from,
				to,
				max_swap,
				None,
				SwapType::Swap(ForeignChainAddress::Eth(Default::default())),
			)]
		);
		assert_eq!(CollectedRejectedFunds::<Test>::get(from), 900);
	});
}

#[test]
fn max_swap_amount_can_be_removed() {
	new_test_ext().execute_with(|| {
		let max_swap = 100;
		let amount = 1_000;
		let from: Asset = Asset::Usdc;
		let to: Asset = Asset::Flip;

		// Initial max swap amount is set.
		set_maximum_swap_amount(from, Some(max_swap));
		assert_ok!(Swapping::schedule_swap_from_contract(
			RuntimeOrigin::signed(ALICE),
			from,
			to,
			amount,
			EncodedAddress::Eth(Default::default()),
			Default::default(),
		));

		assert_eq!(CollectedRejectedFunds::<Test>::get(from), 900u128);

		// Reset event and confiscated funds.
		CollectedRejectedFunds::<Test>::set(from, 0u128);
		System::reset_events();

		// Max is removed.
		set_maximum_swap_amount(from, None);

		assert_ok!(Swapping::schedule_swap_from_contract(
			RuntimeOrigin::signed(ALICE),
			from,
			to,
			amount,
			EncodedAddress::Eth(Default::default()),
			Default::default(),
		));

		let execute_at = System::block_number() + u64::from(SWAP_DELAY_BLOCKS);

		assert_eq!(
			SwapQueue::<Test>::get(execute_at),
			vec![
				Swap::new(
					1,
					from,
					to,
					max_swap,
					None,
					SwapType::Swap(ForeignChainAddress::Eth(Default::default())),
				),
				// New swap takes the full amount.
				Swap::new(
					2,
					from,
					to,
					amount,
					None,
					SwapType::Swap(ForeignChainAddress::Eth(Default::default())),
				),
			]
		);
		// No no funds are confiscated.
		assert_eq!(CollectedRejectedFunds::<Test>::get(from), 0);
	});
}

#[test]
fn swap_input_excludes_network_fee() {
	const AMOUNT: AssetAmount = 1_000;
	const FROM_ASSET: Asset = Asset::Usdc;
	const TO_ASSET: Asset = Asset::Flip;
	let destination_address: ForeignChainAddress = ForeignChainAddress::Eth(Default::default());
	const NETWORK_FEE: Permill = Permill::from_percent(1);

	NetworkFee::set(NETWORK_FEE);

	new_test_ext()
		.execute_with(|| {
			Swapping::schedule_swap_from_channel(
				ForeignChainAddress::Eth(Default::default()),
				0,
				FROM_ASSET,
				TO_ASSET,
				AMOUNT,
				destination_address.clone(),
				bounded_vec![],
				None,
				0,
			);
		})
		.then_process_blocks_until(|_| System::block_number() == 3)
		.then_execute_with(|_| {
			let expected_swap_input = AMOUNT - NETWORK_FEE * AMOUNT;

			System::assert_has_event(RuntimeEvent::Swapping(Event::<Test>::SwapExecuted {
				swap_id: 1,
				source_asset: FROM_ASSET,
				destination_asset: TO_ASSET,
				deposit_amount: expected_swap_input,
				egress_amount: expected_swap_input,
				swap_input: expected_swap_input,
				swap_output: expected_swap_input,
				intermediate_amount: None,
				swap_type: SwapType::Swap(destination_address),
			}));
		});

	NetworkFee::set(Default::default());
}

#[test]
fn can_swap_below_max_amount() {
	new_test_ext().execute_with(|| {
		let max_swap = 1_001u128;
		let amount = 1_000u128;
		let from: Asset = Asset::Usdc;
		let to: Asset = Asset::Flip;

		// Initial max swap amount is set.
		set_maximum_swap_amount(from, Some(max_swap));
		assert_ok!(Swapping::schedule_swap_from_contract(
			RuntimeOrigin::signed(ALICE),
			from,
			to,
			amount,
			EncodedAddress::Eth(Default::default()),
			Default::default(),
		));

		assert_eq!(CollectedRejectedFunds::<Test>::get(from), 0u128);

		assert_eq!(
			SwapQueue::<Test>::get(System::block_number() + u64::from(SWAP_DELAY_BLOCKS)),
			vec![Swap::new(
				1,
				from,
				to,
				amount,
				None,
				SwapType::Swap(ForeignChainAddress::Eth(Default::default())),
			),]
		);
	});
}

#[test]
fn can_swap_ccm_below_max_amount() {
	new_test_ext().execute_with(|| {
		let gas_budget = GAS_BUDGET;
		let principal_amount = 999;
		let max_swap = 1_001;
		let from: Asset = Asset::Usdc;
		let to: Asset = Asset::Flip;
		let ccm = generate_ccm_deposit();

		set_maximum_swap_amount(from, Some(max_swap));

		// Register CCM via Swap deposit
		assert_ok!(Swapping::ccm_deposit(
			RuntimeOrigin::root(),
			from,
			gas_budget + principal_amount,
			to,
			EncodedAddress::Eth(Default::default()),
			ccm,
			Default::default(),
		));

		let execute_at = System::block_number() + u64::from(SWAP_DELAY_BLOCKS);

		assert_eq!(
			SwapQueue::<Test>::get(execute_at),
			vec![
				Swap::new(1, from, to, principal_amount, None, SwapType::CcmPrincipal(1),),
				Swap::new(2, from, Asset::Eth, gas_budget, None, SwapType::CcmGas(1),)
			]
		);
		assert_eq!(CollectedRejectedFunds::<Test>::get(from), 0);
	});
}

fn swap_with_custom_broker_fee(
	from: Asset,
	to: Asset,
	amount: AssetAmount,
	broker_fee: BasisPoints,
) {
	<Pallet<Test> as SwapDepositHandler>::schedule_swap_from_channel(
		ForeignChainAddress::Eth([2; 20].into()),
		Default::default(),
		from,
		to,
		amount,
		ForeignChainAddress::Eth([2; 20].into()),
		bounded_vec![Beneficiary { account: ALICE, bps: broker_fee }],
		None,
		1,
	);
}

#[test]
fn swap_broker_fee_calculated_correctly() {
	new_test_ext().execute_with(|| {
		let fees: [BasisPoints; 12] =
			[1, 5, 10, 100, 200, 500, 1000, 1500, 2000, 5000, 7500, 10000];
		const AMOUNT: AssetAmount = 100000;

		// calculate broker fees for each asset available
		Asset::all().for_each(|asset| {
			let total_fees: u128 =
				fees.iter().fold(0, |total_fees: u128, fee_bps: &BasisPoints| {
					swap_with_custom_broker_fee(asset, Asset::Usdc, AMOUNT, *fee_bps);
					total_fees +
						Permill::from_parts(*fee_bps as u32 * BASIS_POINTS_PER_MILLION) * AMOUNT
				});
			assert_eq!(EarnedBrokerFees::<Test>::get(ALICE, asset), total_fees);
		});
	});
}

#[test]
fn swap_broker_fee_cannot_exceed_amount() {
	new_test_ext().execute_with(|| {
		swap_with_custom_broker_fee(Asset::Usdc, Asset::Flip, 100, 15000);
		assert_eq!(EarnedBrokerFees::<Test>::get(ALICE, cf_primitives::Asset::Usdc), 100);
	});
}

fn assert_swap_scheduled_event_emitted(
	swap_id: u64,
	source_asset: Asset,
	deposit_amount: AssetAmount,
	destination_asset: Asset,
	broker_commission: AssetAmount,
	execute_at: u64,
) {
	System::assert_last_event(RuntimeEvent::Swapping(Event::<Test>::SwapScheduled {
		swap_id,
		source_asset,
		deposit_amount,
		destination_asset,
		destination_address: EncodedAddress::Eth([2; 20]),
		origin: SwapOrigin::DepositChannel {
			deposit_address: EncodedAddress::Eth([2; 20]),
			channel_id: 1,
			deposit_block_height: Default::default(),
		},
		swap_type: SwapType::Swap(ForeignChainAddress::Eth([2; 20].into())),
		broker_commission: Some(broker_commission),
		broker_fee: Some(broker_commission),
		execute_at,
	}));
}
#[test]
fn swap_broker_fee_subtracted_from_swap_amount() {
	new_test_ext().execute_with(|| {
		let amounts: [AssetAmount; 6] = [50, 100, 200, 500, 1000, 10000];
		let fees: [BasisPoints; 4] = [100, 1000, 5000, 10000];

		let combinations = amounts.iter().cartesian_product(fees);

		let execute_at = System::block_number() + SWAP_DELAY_BLOCKS as u64;

		let mut swap_id = 1;
		Asset::all().for_each(|asset| {
			let mut total_fees = 0;
			combinations.clone().for_each(|(amount, broker_fee)| {
				swap_with_custom_broker_fee(asset, Asset::Flip, *amount, broker_fee);
				let broker_commission =
					Permill::from_parts(broker_fee as u32 * BASIS_POINTS_PER_MILLION) * *amount;
				total_fees += broker_commission;
				assert_eq!(EarnedBrokerFees::<Test>::get(ALICE, asset), total_fees);
				assert_swap_scheduled_event_emitted(
					swap_id,
					asset,
					*amount,
					Asset::Flip,
					broker_commission,
					execute_at,
				);
				swap_id += 1;
			})
		});
	});
}

#[test]
fn broker_bps_is_limited() {
	new_test_ext().execute_with(|| {
		assert_noop!(
			Swapping::request_swap_deposit_address_with_affiliates(
				RuntimeOrigin::signed(ALICE),
				Asset::Eth,
				Asset::Usdc,
				EncodedAddress::Eth(Default::default()),
				1001,
				None,
				0,
				Default::default(),
				None,
			),
			Error::<Test>::BrokerCommissionBpsTooHigh
		);
	});
}

#[test]
fn swaps_are_executed_according_to_execute_at_field() {
	let mut swaps = generate_test_swaps();
	let later_swaps = swaps.split_off(2);

	new_test_ext()
		.execute_with(|| {
			// Block 1, swaps should be scheduled at block 3
			assert_eq!(System::block_number(), 1);
			insert_swaps(&swaps);

			assert_event_sequence!(
				Test,
				RuntimeEvent::Swapping(Event::SwapScheduled { swap_id: 1, execute_at: 3, .. }),
				RuntimeEvent::Swapping(Event::SwapScheduled { swap_id: 2, execute_at: 3, .. }),
			);
		})
		.then_execute_at_next_block(|_| {
			// Block 2, swaps should be scheduled at block 4
			assert_eq!(System::block_number(), 2);
			insert_swaps(&later_swaps);
			assert_event_sequence!(
				Test,
				RuntimeEvent::Swapping(Event::SwapScheduled { swap_id: 3, execute_at: 4, .. }),
				RuntimeEvent::Swapping(Event::SwapScheduled { swap_id: 4, execute_at: 4, .. }),
			);
		})
		.then_execute_at_next_block(|_| {
			// First group of swaps will be processed at the end of this block
		})
		.then_execute_with(|_| {
			assert_eq!(System::block_number(), 3);
			assert_event_sequence!(
				Test,
				RuntimeEvent::Swapping(Event::<Test>::NetworkFeeTaken { swap_id: 1, .. }),
				RuntimeEvent::Swapping(Event::<Test>::NetworkFeeTaken { swap_id: 2, .. }),
				RuntimeEvent::Swapping(Event::SwapExecuted { swap_id: 1, .. }),
				RuntimeEvent::Swapping(Event::SwapEgressScheduled { swap_id: 1, .. }),
				RuntimeEvent::Swapping(Event::SwapExecuted { swap_id: 2, .. }),
				RuntimeEvent::Swapping(Event::SwapEgressScheduled { swap_id: 2, .. }),
			);
		})
		.then_execute_at_next_block(|_| {
			// Second group of swaps will be processed at the end of this block
		})
		.then_execute_with(|_| {
			assert_eq!(System::block_number(), 4);
			assert_event_sequence!(
				Test,
				RuntimeEvent::Swapping(Event::<Test>::NetworkFeeTaken { swap_id: 3, .. }),
				RuntimeEvent::Swapping(Event::<Test>::NetworkFeeTaken { swap_id: 4, .. }),
				RuntimeEvent::Swapping(Event::SwapExecuted { swap_id: 3, .. }),
				RuntimeEvent::Swapping(Event::SwapEgressScheduled { swap_id: 3, .. }),
				RuntimeEvent::Swapping(Event::SwapExecuted { swap_id: 4, .. }),
				RuntimeEvent::Swapping(Event::SwapEgressScheduled { swap_id: 4, .. }),
			);
		});
}

#[test]
fn swaps_get_retried_after_failure() {
	let mut swaps = generate_test_swaps();
	let later_swaps = swaps.split_off(2);

	const EXECUTE_AT_BLOCK: u64 = 3;
	const RETRY_AT_BLOCK: u64 = EXECUTE_AT_BLOCK + DEFAULT_SWAP_RETRY_DELAY_BLOCKS;

	new_test_ext()
		.execute_with(|| {
			// Block 1, swaps should be scheduled at block 3
			assert_eq!(System::block_number(), 1);
			insert_swaps(&swaps);

			assert_event_sequence!(
				Test,
				RuntimeEvent::Swapping(Event::SwapScheduled {
					swap_id: 1,
					execute_at: EXECUTE_AT_BLOCK,
					..
				}),
				RuntimeEvent::Swapping(Event::SwapScheduled {
					swap_id: 2,
					execute_at: EXECUTE_AT_BLOCK,
					..
				}),
			);
		})
		.then_execute_at_next_block(|_| {
			// Block 2, swaps should be scheduled at block 4
			assert_eq!(System::block_number(), 2);
			insert_swaps(&later_swaps);
			assert_event_sequence!(
				Test,
				RuntimeEvent::Swapping(Event::SwapScheduled { swap_id: 3, execute_at: 4, .. }),
				RuntimeEvent::Swapping(Event::SwapScheduled { swap_id: 4, execute_at: 4, .. }),
			);
		})
		.then_execute_at_next_block(|_| {
			// First group of swaps will be processed at the end of this block,
			// but we force them to fail:
			MockSwappingApi::set_swaps_should_fail(true);
		})
		.then_execute_with(|_| {
			assert_eq!(System::block_number(), 3);
			assert_event_sequence!(
				Test,
				RuntimeEvent::Swapping(Event::BatchSwapFailed { .. }),
				RuntimeEvent::Swapping(Event::SwapRescheduled {
					swap_id: 1,
					execute_at: RETRY_AT_BLOCK
				}),
				RuntimeEvent::Swapping(Event::SwapRescheduled {
					swap_id: 2,
					execute_at: RETRY_AT_BLOCK
				}),
			);

			assert_eq!(SwapQueue::<Test>::get(RETRY_AT_BLOCK).len(), 2);
		})
		.then_execute_at_next_block(|_| {
			assert_eq!(System::block_number(), 4);
			// The swaps originally scheduled for block 4 should be executed now,
			// and should succeed.
			MockSwappingApi::set_swaps_should_fail(false);
		})
		.then_execute_with(|_| {
			assert_event_sequence!(
				Test,
				RuntimeEvent::Swapping(Event::NetworkFeeTaken { swap_id: 3, .. }),
				RuntimeEvent::Swapping(Event::NetworkFeeTaken { swap_id: 4, .. }),
				RuntimeEvent::Swapping(Event::SwapExecuted { swap_id: 3, .. }),
				RuntimeEvent::Swapping(Event::SwapEgressScheduled { swap_id: 3, .. }),
				RuntimeEvent::Swapping(Event::SwapExecuted { swap_id: 4, .. }),
				RuntimeEvent::Swapping(Event::SwapEgressScheduled { swap_id: 4, .. }),
			);
		})
		.then_execute_at_block(RETRY_AT_BLOCK, |_| {})
		.then_execute_with(|_| {
			// Re-trying failed swaps originally scheduled for block 3 (which should
			// now be successful):
			assert_event_sequence!(
				Test,
				RuntimeEvent::Swapping(Event::NetworkFeeTaken { swap_id: 1, .. }),
				RuntimeEvent::Swapping(Event::NetworkFeeTaken { swap_id: 2, .. }),
				RuntimeEvent::Swapping(Event::SwapExecuted { swap_id: 1, .. }),
				RuntimeEvent::Swapping(Event::SwapEgressScheduled { swap_id: 1, .. }),
				RuntimeEvent::Swapping(Event::SwapExecuted { swap_id: 2, .. }),
				RuntimeEvent::Swapping(Event::SwapEgressScheduled { swap_id: 2, .. }),
			);
		});
}

#[test]
fn deposit_address_ready_event_contains_correct_parameters() {
	new_test_ext().execute_with(|| {
		let refund_parameters = ChannelRefundParameters {
			retry_duration: 10,
			refund_address: ForeignChainAddress::Eth([10; 20].into()),
			min_price: 100.into(),
		};

		const BOOST_FEE: u16 = 100;
		assert_ok!(Swapping::request_swap_deposit_address_with_affiliates(
			RuntimeOrigin::signed(ALICE),
			Asset::Eth,
			Asset::Usdc,
			EncodedAddress::Eth(Default::default()),
			0,
			None,
			BOOST_FEE,
			Default::default(),
			Some(refund_parameters.clone()),
		));
		assert_event_sequence!(
			Test,
			RuntimeEvent::Swapping(Event::SwapDepositAddressReady {
				boost_fee: BOOST_FEE,
				refund_parameters: Some(ref refund_params_in_event),
				..
			}) if refund_params_in_event == &refund_parameters
		);
	});
}

#[test]
fn test_get_scheduled_swap_legs() {
	new_test_ext().execute_with(|| {
		const SWAP_TYPE: SwapType = SwapType::Swap(ForeignChainAddress::Eth(H160::zero()));
		const INIT_AMOUNT: AssetAmount = 1000;

		let swaps: Vec<_> = [
			(1, Asset::Flip, Asset::Usdc),
			(2, Asset::Usdc, Asset::Flip),
			(3, Asset::Btc, Asset::Eth),
			(4, Asset::Flip, Asset::Btc),
			(5, Asset::Eth, Asset::Flip),
		]
		.into_iter()
		.map(|(id, from, to)| Swap::new(id, from, to, INIT_AMOUNT, None, SWAP_TYPE.clone()))
		.collect();

		SwapRate::set(2f64);
		// The amount of USDC in the middle of swap (5):
		const INTERMEDIATE_AMOUNT: AssetAmount = 2000;

		// The test is more useful when these aren't equal:
		assert_ne!(INIT_AMOUNT, INTERMEDIATE_AMOUNT);

		assert_eq!(
			Swapping::get_scheduled_swap_legs(swaps, Asset::Flip, None),
			vec![
				SwapLegInfo {
					swap_id: 1,
					base_asset: Asset::Flip,
					quote_asset: Asset::Usdc,
					side: Side::Sell,
					amount: INIT_AMOUNT,
					source_asset: None,
					source_amount: None,
				},
				SwapLegInfo {
					swap_id: 2,
					base_asset: Asset::Flip,
					quote_asset: Asset::Usdc,
					side: Side::Buy,
					amount: INIT_AMOUNT,
					source_asset: None,
					source_amount: None,
				},
				SwapLegInfo {
					swap_id: 4,
					base_asset: Asset::Flip,
					quote_asset: Asset::Usdc,
					side: Side::Sell,
					amount: INIT_AMOUNT,
					source_asset: None,
					source_amount: None,
				},
				SwapLegInfo {
					swap_id: 5,
					base_asset: Asset::Flip,
					quote_asset: Asset::Usdc,
					side: Side::Buy,
					amount: INTERMEDIATE_AMOUNT,
					source_asset: Some(Asset::Eth),
					source_amount: Some(INIT_AMOUNT),
				},
			]
		);
	});
}

#[test]
fn test_get_scheduled_swap_legs_fallback() {
	new_test_ext().execute_with(|| {
		const SWAP_TYPE: SwapType = SwapType::Swap(ForeignChainAddress::Eth(H160::zero()));
		const INIT_AMOUNT: AssetAmount = 1000000000000000000000;
		const PRICE: u128 = 2;

		let swaps: Vec<_> = [(1, Asset::Flip, Asset::Eth), (2, Asset::Eth, Asset::Usdc)]
			.into_iter()
			.map(|(id, from, to)| Swap::new(id, from, to, INIT_AMOUNT, None, SWAP_TYPE.clone()))
			.collect();

		// Setting the swap rate to something different from the price so that if the fallback is
		// not used, it will give a different result, avoiding a false positive.
		SwapRate::set(PRICE.checked_add(1).unwrap() as f64);

		// The swap simulation must fail for it to use the fallback price estimation
		MockSwappingApi::set_swaps_should_fail(true);

		let sqrt_price = price_to_sqrt_price((U256::from(PRICE)) << PRICE_FRACTIONAL_BITS);

		assert_eq!(
			Swapping::get_scheduled_swap_legs(swaps, Asset::Eth, Some(sqrt_price)),
			vec![
				SwapLegInfo {
					swap_id: 1,
					base_asset: Asset::Eth,
					quote_asset: Asset::Usdc,
					side: Side::Buy,
					amount: INIT_AMOUNT * PRICE,
					source_asset: Some(Asset::Flip),
					source_amount: Some(INIT_AMOUNT),
				},
				SwapLegInfo {
					swap_id: 2,
					base_asset: Asset::Eth,
					quote_asset: Asset::Usdc,
					side: Side::Sell,
					amount: INIT_AMOUNT,
					source_asset: None,
					source_amount: None,
				}
			]
		);
	});
}

#[test]
fn can_update_all_config_items() {
	new_test_ext().execute_with(|| {
		const NEW_MAX_SWAP_AMOUNT_BTC: Option<AssetAmount> = Some(100);
		const NEW_MAX_SWAP_AMOUNT_DOT: Option<AssetAmount> = Some(69);
		let new_swap_retry_delay = BlockNumberFor::<Test>::from(1234u32);
		let new_flip_buy_interval = BlockNumberFor::<Test>::from(5678u32);

		// Check that the default values are different from the new ones
		assert!(MaximumSwapAmount::<Test>::get(Asset::Btc).is_none());
		assert!(MaximumSwapAmount::<Test>::get(Asset::Dot).is_none());
		assert_ne!(SwapRetryDelay::<Test>::get(), new_swap_retry_delay);
		assert_ne!(FlipBuyInterval::<Test>::get(), new_flip_buy_interval);

		// Update all config items at the same time, and updates 2 separate max swap amounts.
		assert_ok!(Swapping::update_pallet_config(
			OriginTrait::root(),
			vec![
				PalletConfigUpdate::MaximumSwapAmount {
					asset: Asset::Btc,
					amount: NEW_MAX_SWAP_AMOUNT_BTC
				},
				PalletConfigUpdate::MaximumSwapAmount {
					asset: Asset::Dot,
					amount: NEW_MAX_SWAP_AMOUNT_DOT
				},
				PalletConfigUpdate::SwapRetryDelay { delay: new_swap_retry_delay },
				PalletConfigUpdate::FlipBuyInterval { interval: new_flip_buy_interval },
			]
			.try_into()
			.unwrap()
		));

		// Check that the new values were set
		assert_eq!(MaximumSwapAmount::<Test>::get(Asset::Btc), NEW_MAX_SWAP_AMOUNT_BTC);
		assert_eq!(MaximumSwapAmount::<Test>::get(Asset::Dot), NEW_MAX_SWAP_AMOUNT_DOT);
		assert_eq!(SwapRetryDelay::<Test>::get(), new_swap_retry_delay);
		assert_eq!(FlipBuyInterval::<Test>::get(), new_flip_buy_interval);

		// Check that the events were emitted
		assert_events_eq!(
			Test,
			RuntimeEvent::Swapping(crate::Event::MaximumSwapAmountSet {
				asset: Asset::Btc,
				amount: NEW_MAX_SWAP_AMOUNT_BTC,
			}),
			RuntimeEvent::Swapping(crate::Event::MaximumSwapAmountSet {
				asset: Asset::Dot,
				amount: NEW_MAX_SWAP_AMOUNT_DOT,
			}),
			RuntimeEvent::Swapping(crate::Event::SwapRetryDelaySet {
				swap_retry_delay: new_swap_retry_delay
			}),
			RuntimeEvent::Swapping(crate::Event::BuyIntervalSet {
				buy_interval: new_flip_buy_interval
			})
		);
	});
}

#[test]
fn network_fee_swap_gets_burnt() {
	new_test_ext().execute_with(|| {
		const AMOUNT: AssetAmount = 100;

		Swapping::schedule_swap(Asset::Usdc, Asset::Flip, AMOUNT, None, SwapType::NetworkFee);
		assert_eq!(FlipToBurn::<Test>::get(), 0);

		Swapping::on_finalize(System::block_number() + SWAP_DELAY_BLOCKS as u64);
		assert_swaps_queue_is_empty();
		assert_eq!(FlipToBurn::<Test>::get(), AMOUNT);
	});
}

#[test]
fn transaction_fees_are_collected() {
	new_test_ext().execute_with(|| {
		const AMOUNT: AssetAmount = 100;

		Swapping::schedule_swap(Asset::Flip, Asset::Eth, AMOUNT, None, SwapType::IngressEgressFee);
		assert_eq!(
			MockIngressEgressFeeHandler::<Ethereum>::withheld_assets(
				cf_chains::assets::eth::GAS_ASSET
			),
			0
		);

		Swapping::on_finalize(System::block_number() + SWAP_DELAY_BLOCKS as u64);
		assert_swaps_queue_is_empty();
		assert_eq!(
			MockIngressEgressFeeHandler::<Ethereum>::withheld_assets(
				cf_chains::assets::eth::GAS_ASSET
			),
			AMOUNT
		);
	});
}

#[test]
fn register_and_deregister_account() {
	new_test_ext().execute_with(|| {
		<<Test as Chainflip>::AccountRoleRegistry as AccountRoleRegistry<Test>>::ensure_broker(
			OriginTrait::signed(ALICE),
		)
		.expect("ALICE was registered in test setup.");

		// Earn some fees.
		EarnedBrokerFees::<Test>::insert(ALICE, Asset::Eth, 100);

		assert_noop!(
			Swapping::deregister_as_broker(OriginTrait::signed(ALICE)),
			Error::<Test>::EarnedFeesNotWithdrawn,
		);

		assert_ok!(Swapping::withdraw(
			OriginTrait::signed(ALICE),
			Asset::Eth,
			EncodedAddress::Eth(Default::default()),
		));
		assert_ok!(Swapping::deregister_as_broker(OriginTrait::signed(ALICE)),);

		assert!(!EarnedBrokerFees::<Test>::contains_key(ALICE, Asset::Eth));
		<<Test as Chainflip>::AccountRoleRegistry as AccountRoleRegistry<Test>>::ensure_broker(
			OriginTrait::signed(ALICE),
		)
		.expect_err("ALICE should be deregistered.");
	});
}

#[test]
fn swap_output_amounts_correctly_account_for_fees() {
	for (from, to) in
		// non-stable to non-stable, non-stable to stable, stable to non-stable
		[(Asset::Btc, Asset::Eth), (Asset::Btc, Asset::Usdc), (Asset::Usdc, Asset::Eth)]
	{
		new_test_ext().execute_with(|| {
			const SWAPPED_AMOUNT: AssetAmount = 1000;

			let network_fee = Permill::from_percent(1);
			NetworkFee::set(network_fee);

			let expected_output: AssetAmount =
				(SWAPPED_AMOUNT as u32 - (network_fee * SWAPPED_AMOUNT as u32)).into();

			{
				Swapping::schedule_swap(
					from,
					to,
					SWAPPED_AMOUNT,
					None,
					SwapType::Swap(ForeignChainAddress::Eth(H160::zero())),
				);

				Swapping::on_finalize(System::block_number() + SWAP_DELAY_BLOCKS as u64);

				assert_eq!(
					MockEgressHandler::<AnyChain>::get_scheduled_egresses(),
					vec![MockEgressParameter::Swap {
						asset: to,
						amount: expected_output,
						fee: 0,
						destination_address: ForeignChainAddress::Eth(H160::zero()),
					},]
				);
			}
		});
	}
}

#[test]
fn test_buy_back_flip() {
	new_test_ext().execute_with(|| {
		const INTERVAL: BlockNumberFor<Test> = 5;
		const SWAP_AMOUNT: AssetAmount = 1000;
		const NETWORK_FEE: Permill = Permill::from_percent(2);

		NetworkFee::set(NETWORK_FEE);

		// Get some network fees, just like we did a swap.
		let NetworkFeeTaken { remaining_amount, network_fee } =
			Swapping::take_network_fee(SWAP_AMOUNT);

		// Sanity check the network fee.
		assert_eq!(network_fee, CollectedNetworkFee::<Test>::get());
		assert_eq!(network_fee, 20);
		assert_eq!(remaining_amount + network_fee, SWAP_AMOUNT);

		// The default buy interval is zero. Check that buy back is disabled & on_initialize does
		// not panic.
		assert_eq!(FlipBuyInterval::<Test>::get(), 0);
		Swapping::on_initialize(1);
		assert_eq!(network_fee, CollectedNetworkFee::<Test>::get());

		// Set a non-zero buy interval
		FlipBuyInterval::<Test>::set(INTERVAL);

		// Nothing is bought if we're not at the interval.
		Swapping::on_initialize(INTERVAL * 3 - 1);
		assert_eq!(network_fee, CollectedNetworkFee::<Test>::get());

		// If we're at an interval, we should buy flip.
		Swapping::on_initialize(INTERVAL * 3);
		assert_eq!(0, CollectedNetworkFee::<Test>::get());
		assert_eq!(
			SwapQueue::<Test>::get(System::block_number() + u64::from(SWAP_DELAY_BLOCKS))
				.first()
				.expect("Should have scheduled a swap usdc -> flip"),
			&Swap::new(1, STABLE_ASSET, Asset::Flip, network_fee, None, SwapType::NetworkFee)
		);
	});
}

#[test]
fn test_network_fee_calculation() {
	new_test_ext().execute_with(|| {
		// Show we can never overflow and panic
		utilities::calculate_network_fee(Permill::from_percent(100), AssetAmount::MAX);
		// 200 bps (2%) of 100 = 2
		assert_eq!(utilities::calculate_network_fee(Permill::from_percent(2u32), 100), (98, 2));
		// 2220 bps = 22 % of 199 = 43,78
		assert_eq!(
			utilities::calculate_network_fee(Permill::from_rational(2220u32, 10000u32), 199),
			(155, 44)
		);
		// 2220 bps = 22 % of 234 = 51,26
		assert_eq!(
			utilities::calculate_network_fee(Permill::from_rational(2220u32, 10000u32), 233),
			(181, 52)
		);
		// 10 bps = 0,1% of 3000 = 3
		assert_eq!(
			utilities::calculate_network_fee(Permill::from_rational(1u32, 1000u32), 3000),
			(2997, 3)
		);
	});
}

#[test]
fn test_calculate_input_for_gas_output() {
	use cf_chains::assets::eth::Asset as EthereumAsset;
	const FLIP: EthereumAsset = EthereumAsset::Flip;

	new_test_ext().execute_with(|| {
		// If swap simulation fails -> no conversion.
		MockSwappingApi::set_swaps_should_fail(true);
		assert!(Swapping::calculate_input_for_gas_output::<Ethereum>(FLIP, 1000).is_none());

		// Set swap rate to 2 and turn swaps back on.
		SwapRate::set(2_f64);
		MockSwappingApi::set_swaps_should_fail(false);

		// Desired output is zero -> trivially ok.
		assert_eq!(Swapping::calculate_input_for_gas_output::<Ethereum>(FLIP, 0), Some(0));

		// Desired output requires 2 swap legs, each with a swap rate of 2. So output should be
		// 1/4th of input.
		assert_eq!(Swapping::calculate_input_for_gas_output::<Ethereum>(FLIP, 1000), Some(250));

		// Desired output is gas asset, requires 1 swap leg. So output should be 1/2 of input.
		assert_eq!(
			Swapping::calculate_input_for_gas_output::<Ethereum>(EthereumAsset::Usdc, 1000),
			Some(500)
		);

		// Input is gas asset -> trivially ok.
		assert_eq!(
			Swapping::calculate_input_for_gas_output::<Ethereum>(
				cf_chains::assets::eth::GAS_ASSET,
				1000
			),
			Some(1000)
		);
	});
}

#[test]
fn test_fee_estimation_basis() {
	for asset in Asset::all() {
		if !asset.is_gas_asset() {
			assert!(
				utilities::fee_estimation_basis(asset).is_some(),
	             "No fee estimation cap defined for {:?}. Add one to the fee_estimation_basis function definition.",
	             asset,
	         );
		}
	}
}
