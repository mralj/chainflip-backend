use crate::Pallet;
use cf_runtime_upgrade_utilities::{PlaceholderMigration, VersionedMigration};
mod add_refund_params;
pub mod remove_prewitnessed_deposits;
pub mod withheld_transaction_fees;

pub type PalletMigration<T, I> = (
	VersionedMigration<Pallet<T, I>, remove_prewitnessed_deposits::Migration<T, I>, 8, 9>,
	VersionedMigration<Pallet<T, I>, add_refund_params::Migration<T, I>, 9, 10>,
	VersionedMigration<Pallet<T, I>, withheld_transaction_fees::Migration<T, I>, 10, 11>,
	PlaceholderMigration<Pallet<T, I>, 11>,
);
