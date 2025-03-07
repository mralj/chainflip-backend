use cf_utilities::task_scope::{task_scope, Scope};
use chainflip_api::{
	self,
	primitives::{AccountRole, Affiliates, Asset, BasisPoints, CcmChannelMetadata},
	settings::StateChain,
	AccountId32, AddressString, BrokerApi, OperatorApi, RefundParameters, StateChainApi,
	SwapDepositAddress, WithdrawFeesDetail,
};
use clap::Parser;
use futures::FutureExt;
use jsonrpsee::{
	core::{async_trait, RpcResult},
	proc_macros::rpc,
	server::ServerBuilder,
};
use std::path::PathBuf;
use tracing::log;

#[rpc(server, client, namespace = "broker")]
pub trait Rpc {
	#[method(name = "register_account", aliases = ["broker_registerAccount"])]
	async fn register_account(&self) -> RpcResult<String>;

	#[method(name = "request_swap_deposit_address", aliases = ["broker_requestSwapDepositAddress"])]
	async fn request_swap_deposit_address(
		&self,
		source_asset: Asset,
		destination_asset: Asset,
		destination_address: AddressString,
		broker_commission: BasisPoints,
		channel_metadata: Option<CcmChannelMetadata>,
		boost_fee: Option<BasisPoints>,
		affiliate_fees: Option<Affiliates<AccountId32>>,
		refund_parameters: Option<RefundParameters>,
	) -> RpcResult<SwapDepositAddress>;

	#[method(name = "withdraw_fees", aliases = ["broker_withdrawFees"])]
	async fn withdraw_fees(
		&self,
		asset: Asset,
		destination_address: AddressString,
	) -> RpcResult<WithdrawFeesDetail>;
}

pub struct RpcServerImpl {
	api: StateChainApi,
}

impl RpcServerImpl {
	pub async fn new(
		scope: &Scope<'_, anyhow::Error>,
		BrokerOptions { ws_endpoint, signing_key_file, .. }: BrokerOptions,
	) -> Result<Self, anyhow::Error> {
		Ok(Self {
			api: StateChainApi::connect(scope, StateChain { ws_endpoint, signing_key_file })
				.await?,
		})
	}
}

#[async_trait]
impl RpcServer for RpcServerImpl {
	async fn register_account(&self) -> RpcResult<String> {
		Ok(self
			.api
			.operator_api()
			.register_account_role(AccountRole::Broker)
			.await
			.map(|tx_hash| format!("{tx_hash:#x}"))?)
	}

	async fn request_swap_deposit_address(
		&self,
		source_asset: Asset,
		destination_asset: Asset,
		destination_address: AddressString,
		broker_commission: BasisPoints,
		channel_metadata: Option<CcmChannelMetadata>,
		boost_fee: Option<BasisPoints>,
		affiliate_fees: Option<Affiliates<AccountId32>>,
		refund_parameters: Option<RefundParameters>,
	) -> RpcResult<SwapDepositAddress> {
		Ok(self
			.api
			.broker_api()
			.request_swap_deposit_address(
				source_asset,
				destination_asset,
				destination_address,
				broker_commission,
				channel_metadata,
				boost_fee,
				affiliate_fees.unwrap_or_default(),
				refund_parameters,
			)
			.await?)
	}

	async fn withdraw_fees(
		&self,
		asset: Asset,
		destination_address: AddressString,
	) -> RpcResult<WithdrawFeesDetail> {
		Ok(self.api.broker_api().withdraw_fees(asset, destination_address).await?)
	}
}

#[derive(Parser, Debug, Clone, Default)]
#[clap(version = env!("SUBSTRATE_CLI_IMPL_VERSION"), version_short = 'v')]
pub struct BrokerOptions {
	#[clap(
		long = "port",
		default_value = "80",
		help = "The port number on which the broker will listen for connections. Use 0 to assign a random port."
	)]
	pub port: u16,
	#[clap(
		long = "max_connections",
		default_value = "100",
		help = "The maximum number of conncurrent websocket connections to accept."
	)]
	pub max_connections: u32,
	#[clap(
		long = "state_chain.ws_endpoint",
		default_value = "ws://localhost:9944",
		help = "The state chain node's RPC endpoint."
	)]
	pub ws_endpoint: String,
	#[clap(
		long = "state_chain.signing_key_file",
		default_value = "/etc/chainflip/keys/signing_key_file",
		help = "A path to a file that contains the broker's secret key for signing extrinsics."
	)]
	pub signing_key_file: PathBuf,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
	let opts = BrokerOptions::parse();
	chainflip_api::use_chainflip_account_id_encoding();
	tracing_subscriber::FmtSubscriber::builder()
		.with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
		.try_init()
		.expect("setting default subscriber failed");

	task_scope(|scope| {
		async move {
			let server = ServerBuilder::default()
				.max_connections(opts.max_connections)
				.build(format!("0.0.0.0:{}", opts.port))
				.await?;
			let server_addr = server.local_addr()?;
			let server = server.start(RpcServerImpl::new(scope, opts).await?.into_rpc())?;

			log::info!("🎙 Server is listening on {server_addr}.");

			server.stopped().await;

			Ok(())
		}
		.boxed()
	})
	.await
}
