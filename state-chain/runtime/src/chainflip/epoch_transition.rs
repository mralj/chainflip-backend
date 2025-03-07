use crate::AssetBalances;
use cf_primitives::EpochIndex;
use cf_traits::EpochTransitionHandler;

use crate::Witnesser;

pub struct ChainflipEpochTransitions;

impl EpochTransitionHandler for ChainflipEpochTransitions {
	fn on_expired_epoch(expired: EpochIndex) {
		<Witnesser as EpochTransitionHandler>::on_expired_epoch(expired);
	}
	fn on_new_epoch(_new: EpochIndex) {
		AssetBalances::trigger_reconciliation();
	}
}
