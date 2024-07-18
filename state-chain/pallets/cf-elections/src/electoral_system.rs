mod composite;

use cf_primitives::AuthorityCount;
use frame_support::{pallet_prelude::Member, Parameter};

use crate::{
	vote_storage::{PartialOrVote, VoteStorage},
	CorruptStorageError, ElectionIdentifier,
};

/// A trait that describes a method of coming to consensus on some aspect of an external chain.
///
/// Implementations of this trait should *NEVER* directly access the storage of the election pallet,
/// and only access it through the passed-in accessors.
pub trait ElectoralSystem: 'static {
	/// The internal state of the electoral system. This is intended for storing any internal state
	/// of the ElectoralSystem. It is not synchronised and therefore should only be used by the
	/// ElectoralSystem, and not consumed by the engine.
	type ElectoralUnsynchronisedState: Parameter + Member;

	/// Settings of the electoral system. These can be changed at any time by governance, and
	/// are not synchronised with elections, and therefore there is not universal mapping from
	/// elections to these settings values. Therefore it should only be used for internal
	/// state, i.e. the engines should not consume this data.
	///
	/// Also note that if these settings are changed that will not cause election's to be retested.
	type ElectoralUnsynchronisedSettings: Parameter + Member;

	/// Settings of the electoral system. These settings are synchronised with
	/// elections, so all engines will have a consistent view of the electoral settings to use for a
	/// given election.
	type ElectoralSettings: Parameter + Member + Eq;

	/// Extra data stored along with the UniqueMonotonicIdentifier as part of the
	/// ElectionIdentifier. This is used by composite electoral systems to identify which variant of
	/// election it is working with.
	type ElectionIdentifierExtra: Parameter + Member + Copy + Eq + Ord;

	/// The properties of a single election, typically describing which block the election is
	/// associated with and what needs to be witnessed.
	type ElectionProperties: Parameter + Member;

	/// Per-election state needed by the ElectoralSystem. This state is not synchronised across
	/// engines, and may change during the lifetime of a election.
	type ElectionState: Parameter + Member;

	/// A description of the validator's view of the election's topic. For example a list of all
	/// ingresses the validator has observed in the block the election is about.
	type Vote: VoteStorage;

	/// This is the information that results from consensus. Typically this will be the same as the
	/// `Vote` type, but with more complex consensus models the result of an election may not be
	/// sensibly represented in the same form as a single vote.
	type Consensus: Parameter + Member + Eq;

	/// Custom parameters for `on_finalize`. While it gives more flexibility to use a generic
	/// type here, instead of an associated type, I want to avoid spreading additional generics
	/// throughout the rest of the code. As an alternative, you can use dynamic dispatch (i.e.
	/// Box<dyn ...>) to achieve much the same affect.
	type Context;

	/// This is not used by the pallet, but is used to tell a validator that it should attempt
	/// to vote in a given Election. It returns the time until you should vote.
	fn is_vote_desired<ElectionAccess: ElectionReadAccess<Self>>(
		_election_identifier_with_extra: ElectionIdentifier<Self::ElectionIdentifierExtra>,
		_election_access: &ElectionAccess,
		current_vote: Option<(
			<Self::Vote as VoteStorage>::Properties,
			PartialOrVote<
				<Self::Vote as VoteStorage>::PartialVote,
				<Self::Vote as VoteStorage>::Vote,
			>,
		)>,
	) -> Result<Option<core::time::Duration>, CorruptStorageError> {
		Ok(current_vote.is_none().then_some(core::time::Duration::ZERO))
	}

	/// This is used in the vote extrinsic to disallow a validator from providing votes that do not
	/// pass this check.
	fn is_vote_valid<ElectionAccess: ElectionReadAccess<Self>>(
		election_identifier: ElectionIdentifier<Self::ElectionIdentifierExtra>,
		election_access: &ElectionAccess,
		partial_vote: <Self::Vote as VoteStorage>::PartialVote,
	) -> Result<bool, CorruptStorageError>;

	/// This is called every time a vote occurs. It associates the vote with a `Properties`
	/// value.
	fn vote_properties(
		election_identifier: ElectionIdentifier<Self::ElectionIdentifierExtra>,
		previous_vote: Option<(
			<Self::Vote as VoteStorage>::Properties,
			PartialOrVote<
				<Self::Vote as VoteStorage>::PartialVote,
				<Self::Vote as VoteStorage>::Vote,
			>,
		)>,
		vote: &<Self::Vote as VoteStorage>::PartialVote,
	) -> Result<<Self::Vote as VoteStorage>::Properties, CorruptStorageError>;

	/// This is called during the pallet's `on_finalize` callback, if elections aren't paused and
	/// the CorruptStorage error hasn't occurred.
	fn on_finalize<MultiElectionAccess: MultiElectionWriteAccess<Self>>(
		election_identifiers: Vec<ElectionIdentifier<Self::ElectionIdentifierExtra>>,
		election_access: &mut MultiElectionAccess,
		context: &Self::Context,
	) -> Result<(), CorruptStorageError>;

	/// This function determines if the votes we have received form a consensus. It is called as
	/// part of the Election pallet's `on_finalize` callback when the Election's votes or state have
	/// changed since the previous call.
	///
	/// You should *NEVER* update the epoch during this call.
	fn check_consensus<ElectionAccess: ElectionReadAccess<Self>>(
		election_identifier: ElectionIdentifier<Self::ElectionIdentifierExtra>,
		election_access: &ElectionAccess,
		// This is the consensus as of the last time the consensus was checked. Note this is *NOT*
		// the "last" consensus, i.e. this can be `None` even if on some previous check we had
		// consensus, but it was subsequently lost.
		previous_consensus: Option<&Self::Consensus>,
		votes: Vec<(<Self::Vote as VoteStorage>::Properties, <Self::Vote as VoteStorage>::Vote)>,
		authorities: AuthorityCount,
	) -> Result<Option<Self::Consensus>, CorruptStorageError>;
}

mod election_access {
	//! This module contains a set of traits used to access the details of elections. Notably these
	//! don't allow access to the `Vote` details directly, which are passed directly as needed to
	//! `ElectoralSystem` trait. Their access is handled like this so it easier to simulate the
	//! existence of votes, without having to write custom implementations of these traits. This is
	//! useful to allow validators to simulate the existence of votes in pending extrinics.
	//!
	//! We also restrict access to `Vote` details as the underlying storage does not strictly
	//! guarantee that all votes are from current authorities.

	use super::{CorruptStorageError, ElectoralSystem};
	use crate::ElectionIdentifier;

	/// Represents the current consensus, and how it has changed since it was last checked (i.e.
	/// 'check_consensus' was called).
	pub enum ConsensusStatus<Consensus> {
		/// You did not have consensus when previously checked, but now consensus has been gained.
		Gained {
			/// If you previously had consensus, this will be `Some(...)` and will contain the most
			/// recent consensus before now.
			most_recent: Option<Consensus>,
			new: Consensus,
		},
		/// You had consensus when previously checked, but now no longer have consensus.
		Lost { previous: Consensus },
		/// You had consensus when previously checked, but the consensus has now changed.
		Changed { previous: Consensus, new: Consensus },
		/// You had consensus when previously checked, and the consensus has not changed.
		Unchanged { current: Consensus },
		/// You did not have consensus when previously checked, and still do not.
		None,
	}
	impl<Consensus> ConsensusStatus<Consensus> {
		pub fn try_map<T, E, F: Fn(Consensus) -> Result<T, E>>(
			self,
			f: F,
		) -> Result<ConsensusStatus<T>, E> {
			Ok(match self {
				ConsensusStatus::Gained { most_recent, new } => ConsensusStatus::Gained {
					most_recent: most_recent.map(&f).transpose()?,
					new: f(new)?,
				},
				ConsensusStatus::Lost { previous } =>
					ConsensusStatus::Lost { previous: f(previous)? },
				ConsensusStatus::Changed { previous, new } =>
					ConsensusStatus::Changed { previous: f(previous)?, new: f(new)? },
				ConsensusStatus::Unchanged { current } =>
					ConsensusStatus::Unchanged { current: f(current)? },
				ConsensusStatus::None => ConsensusStatus::None,
			})
		}
	}

	/// A trait allowing read access to the details about a single election
	pub trait ElectionReadAccess<ES: ElectoralSystem + ?Sized> {
		fn settings(&self) -> Result<ES::ElectoralSettings, CorruptStorageError>;
		fn properties(&self) -> Result<ES::ElectionProperties, CorruptStorageError>;
		fn state(&self) -> Result<ES::ElectionState, CorruptStorageError>;
	}

	/// A trait allowing write access to the details about a single election
	pub trait ElectionWriteAccess<ES: ElectoralSystem + ?Sized>: ElectionReadAccess<ES> {
		fn set_state(&mut self, state: ES::ElectionState) -> Result<(), CorruptStorageError>;
		fn clear_votes(&mut self);
		fn delete(self);

		/// This returns the current consensus which will always be up to date with the latest
		/// votes/state. This also returns information about the difference in the consensus between
		/// the last call to `check_consensus`.
		fn check_consensus(
			&mut self,
		) -> Result<ConsensusStatus<ES::Consensus>, CorruptStorageError>;
	}

	/// A trait allowing read access to the details about multiple election
	pub trait MultiElectionReadAccess<ES: ElectoralSystem + ?Sized> {
		type ElectionReadAccess<'a>: ElectionReadAccess<ES>
		where
			Self: 'a;

		fn election(
			&self,
			id: ElectionIdentifier<ES::ElectionIdentifierExtra>,
		) -> Result<Self::ElectionReadAccess<'_>, CorruptStorageError>;
		fn unsynchronised_settings(
			&self,
		) -> Result<ES::ElectoralUnsynchronisedSettings, CorruptStorageError>;
		fn unsynchronised_state(
			&self,
		) -> Result<ES::ElectoralUnsynchronisedState, CorruptStorageError>;
	}

	/// A trait allowing write access to the details about multiple election
	pub trait MultiElectionWriteAccess<ES: ElectoralSystem + ?Sized>:
		MultiElectionReadAccess<ES>
	{
		type ElectionWriteAccess<'a>: ElectionWriteAccess<ES>
		where
			Self: 'a;

		fn new_election(
			&mut self,
			extra: ES::ElectionIdentifierExtra,
			properties: ES::ElectionProperties,
			state: ES::ElectionState,
		) -> Result<Self::ElectionWriteAccess<'_>, CorruptStorageError>;
		fn election_mut(
			&mut self,
			id: ElectionIdentifier<ES::ElectionIdentifierExtra>,
		) -> Result<Self::ElectionWriteAccess<'_>, CorruptStorageError>;
		fn set_unsynchronised_state(
			&mut self,
			unsynchronised_state: ES::ElectoralUnsynchronisedState,
		) -> Result<(), CorruptStorageError>;

		fn mutate_unsynchronised_state<
			T,
			F: for<'a> FnOnce(
				&mut Self,
				&'a mut ES::ElectoralUnsynchronisedState,
			) -> Result<T, CorruptStorageError>,
		>(
			&mut self,
			f: F,
		) -> Result<T, CorruptStorageError> {
			let mut unsynchronised_state = self.unsynchronised_state()?;
			let t = f(self, &mut unsynchronised_state)?;
			self.set_unsynchronised_state(unsynchronised_state)?;
			Ok(t)
		}
	}
}
pub(crate) use election_access::{
	ConsensusStatus, ElectionReadAccess, ElectionWriteAccess, MultiElectionReadAccess,
	MultiElectionWriteAccess,
};
