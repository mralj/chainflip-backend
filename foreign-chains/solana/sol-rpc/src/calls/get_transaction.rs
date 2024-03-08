use std::{collections::HashMap, str::FromStr};

use jsonrpsee::rpc_params;
use serde_json::json;

use cf_chains::assets::sol::Asset;
use sol_prim::{Address, Amount, Signature, SlotNumber};

use super::GetTransaction;
use crate::{
	traits::Call,
	types::{Commitment, JsValue},
};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TxMessage {
	pub account_keys: Vec<Address>,
	pub header: HashMap<String, JsValue>,
	pub instructions: Vec<JsValue>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TxInfo {
	pub message: TxMessage,
	pub signatures: Vec<Signature>,

	#[serde(flatten)]
	extra: HashMap<String, JsValue>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LoadedAddresses {
	pub readonly: Vec<Address>,
	pub writable: Vec<Address>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TokenBalance {
	pub account_index: u64,
	pub mint: JsValue,
	pub owner: Option<JsValue>,
	pub program_id: Option<JsValue>,
	pub ui_token_amount: UiTokenAmount,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UiTokenAmount {
	pub amount: JsValue,
	pub decimals: u64,
	pub ui_amount: Amount,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TxMeta {
	pub log_messages: Vec<String>,
	pub err: Option<JsValue>,
	pub pre_balances: Vec<Amount>,
	pub post_balances: Vec<Amount>,
	pub pre_token_balances: Option<Vec<TokenBalance>>,
	pub post_token_balances: Option<Vec<TokenBalance>>,
	pub fee: Amount,
	pub loaded_addresses: LoadedAddresses,

	#[serde(flatten)]
	extra: HashMap<String, JsValue>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Transaction {
	pub slot: SlotNumber,
	pub block_time: u64,
	pub transaction: TxInfo,
	pub meta: TxMeta,
}

impl Call for GetTransaction {
	type Response = Transaction;
	const CALL_METHOD_NAME: &'static str = "getTransaction";
	fn call_params(&self) -> jsonrpsee::core::params::ArrayParams {
		let signature = self.signature.to_string();
		rpc_params![
			signature.as_str(),
			json!({
				"commitment": self.commitment,
			})
		]
	}
}

impl GetTransaction {
	pub fn for_signature(signature: Signature) -> Self {
		Self { signature, commitment: Commitment::Confirmed }
	}
}

impl Transaction {
	pub fn addresses(&self) -> impl Iterator<Item = &Address> + '_ {
		self.transaction.message.account_keys.iter()
	}

	pub fn balances(&self, address: &Address, asset: Asset) -> Option<(Amount, Amount)> {
		let account_idx =
			self.transaction.message.account_keys.iter().position(|a| a == address)?;
		match asset {
			Asset::Sol => Some((
				self.meta.pre_balances.get(account_idx).copied()?,
				self.meta.post_balances.get(account_idx).copied()?,
			)),
			Asset::SolUsdc => {
				// TODO: The USDC should be pulled by the Engine, not hardcoded.
				// TODO: Maybe instead of using "find" use the account_idx checking it's correct
				// TODO: We should probably also check that the of the ATA to be a particular address (our pda).
				//       If the address is correct (pda_ata), the mint_pubkey is correct and the owner (pda) is
				//       correct then it must be correct and derived according to associated token account.
				let hardcoded_mint_pubkey =
					JsValue::from_str("24PNhTaNtomHhoy3fTRaMhAFCRj4uHqhZEEoWrKDbR5p").unwrap();
				Some((
					self.meta
						.pre_token_balances
						.as_ref()
						.and_then(|balances| {
							balances
								.iter()
								.find(|balance| balance.mint == hardcoded_mint_pubkey)
								.map(|balance| balance.ui_token_amount.ui_amount.clone())
						})
						.unwrap_or(0),
					self.meta
						.post_token_balances
						.as_ref()
						.and_then(|balances| {
							balances
								.iter()
								.find(|balance| balance.mint == hardcoded_mint_pubkey)
								.map(|balance| balance.ui_token_amount.ui_amount.clone())
						})
						.unwrap_or(0),
				))
			},
		}
	}
}
