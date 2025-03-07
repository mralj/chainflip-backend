//! Types and functions that are common to Arbitrum.
pub mod api;

pub mod benchmarking;

use crate::{
	evm::{DeploymentStatus, EvmFetchId},
	*,
};
use cf_primitives::chains::assets;
pub use cf_primitives::chains::Arbitrum;
use codec::{Decode, Encode, MaxEncodedLen};
pub use ethabi::{
	ethereum_types::{H256, U256},
	Address, Hash as TxHash, Token, Uint, Word,
};
use frame_support::sp_runtime::{traits::Zero, FixedPointNumber, FixedU64, RuntimeDebug};
use scale_info::TypeInfo;
use serde::{Deserialize, Serialize};
use sp_std::{cmp::min, str};

use self::evm::EvmCrypto;

// Reference constants for the chain spec
pub const CHAIN_ID_MAINNET: u64 = 42161;
pub const CHAIN_ID_ARBITRUM_SEPOLIA: u64 = 421614;

impl Chain for Arbitrum {
	const NAME: &'static str = "Arbitrum";
	const GAS_ASSET: Self::ChainAsset = assets::arb::Asset::ArbEth;
	const WITNESS_PERIOD: Self::ChainBlockNumber = 24;

	type ChainCrypto = EvmCrypto;
	type ChainBlockNumber = u64;
	type ChainAmount = EthAmount;
	type TransactionFee = evm::TransactionFee;
	type TrackedData = ArbitrumTrackedData;
	type ChainAsset = assets::arb::Asset;
	type ChainAssetMap<
		T: Member
			+ Parameter
			+ MaxEncodedLen
			+ Copy
			+ MaybeSerializeDeserialize
			+ BenchmarkValue
			+ FullCodec
			+ Unpin
			+ Default,
	> = assets::arb::AssetMap<T>;
	type ChainAccount = eth::Address;
	type DepositFetchId = EvmFetchId;
	type DepositChannelState = DeploymentStatus;
	type DepositDetails = evm::DepositDetails;
	type Transaction = evm::Transaction;
	type TransactionMetadata = evm::EvmTransactionMetadata;
	type TransactionRef = H256;
	type ReplayProtectionParams = Self::ChainAccount;
	type ReplayProtection = evm::api::EvmReplayProtection;
}

#[derive(
	Copy,
	Clone,
	RuntimeDebug,
	PartialEq,
	Eq,
	Encode,
	Decode,
	MaxEncodedLen,
	TypeInfo,
	Serialize,
	Deserialize,
)]
#[codec(mel_bound())]
pub struct ArbitrumTrackedData {
	pub base_fee: <Arbitrum as Chain>::ChainAmount,
	pub gas_limit_multiplier: FixedU64,
}

impl Default for ArbitrumTrackedData {
	#[track_caller]
	fn default() -> Self {
		panic!("You should not use the default chain tracking, as it's meaningless.")
	}
}

impl ArbitrumTrackedData {
	pub fn max_fee_per_gas(
		&self,
		base_fee_multiplier: FixedU64,
	) -> <Ethereum as Chain>::ChainAmount {
		base_fee_multiplier.saturating_mul_int(self.base_fee)
	}
}

pub mod fees {
	pub const BASE_COST_PER_BATCH: u128 = 60_000;
	pub const GAS_COST_PER_FETCH: u128 = 30_000;
	pub const GAS_COST_PER_TRANSFER_NATIVE: u128 = 20_000;
	pub const GAS_COST_PER_TRANSFER_TOKEN: u128 = 40_000;
}

impl FeeEstimationApi<Arbitrum> for ArbitrumTrackedData {
	fn estimate_ingress_fee(
		&self,
		asset: <Arbitrum as Chain>::ChainAsset,
	) -> <Arbitrum as Chain>::ChainAmount {
		use crate::arb::fees::*;

		// Note: this is taking the egress cost of the swap in the ingress currency (and basing the
		// cost on the ingress chain).
		let gas_cost_per_fetch = BASE_COST_PER_BATCH +
			match asset {
				assets::arb::Asset::ArbEth => Zero::zero(),
				assets::arb::Asset::ArbUsdc => GAS_COST_PER_FETCH,
			};

		self.base_fee
			.saturating_mul(self.gas_limit_multiplier.saturating_mul_int(gas_cost_per_fetch))
	}

	fn estimate_egress_fee(
		&self,
		asset: <Arbitrum as Chain>::ChainAsset,
	) -> <Arbitrum as Chain>::ChainAmount {
		use crate::arb::fees::*;

		let gas_cost_per_transfer = BASE_COST_PER_BATCH +
			match asset {
				assets::arb::Asset::ArbEth => GAS_COST_PER_TRANSFER_NATIVE,
				assets::arb::Asset::ArbUsdc => GAS_COST_PER_TRANSFER_TOKEN,
			};

		self.base_fee
			.saturating_mul(self.gas_limit_multiplier.saturating_mul_int(gas_cost_per_transfer))
	}
}

impl From<&DepositChannel<Arbitrum>> for EvmFetchId {
	fn from(channel: &DepositChannel<Arbitrum>) -> Self {
		match channel.state {
			DeploymentStatus::Undeployed => EvmFetchId::DeployAndFetch(channel.channel_id),
			DeploymentStatus::Pending | DeploymentStatus::Deployed =>
				if channel.asset == assets::arb::Asset::ArbEth {
					EvmFetchId::NotRequired
				} else {
					EvmFetchId::Fetch(channel.address)
				},
		}
	}
}

impl FeeRefundCalculator<Arbitrum> for evm::Transaction {
	fn return_fee_refund(
		&self,
		fee_paid: <Arbitrum as Chain>::TransactionFee,
	) -> <Arbitrum as Chain>::ChainAmount {
		min(
			self.max_fee_per_gas
				.unwrap_or_default()
				.try_into()
				.expect("In practice `max_fee_per_gas` is always less than u128::MAX"),
			fee_paid.effective_gas_price,
		)
		.saturating_mul(fee_paid.gas_used)
	}
}
