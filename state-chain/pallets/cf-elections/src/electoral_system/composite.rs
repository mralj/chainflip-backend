use super::{ElectoralSystem, MultiElectionWriteAccess};

/// Allows the composition of multiple ElectoralSystems while allowing the ability to configure the
/// `on_finalize` behaviour without exposing the internal composite types.
pub struct Composite<S, T> {
	_phantom: core::marker::PhantomData<(T, S)>,
}

// This trait helps make the macro simpler, by avoiding needing to refer to a individual
// ElectoralSystem, and all the ElectoralSystems at the same time.
trait ElectoralSystemTuple {
	type Type;
}
impl<S, T> ElectoralSystemTuple for Composite<S, T> {
	type Type = T;
}

pub trait ElectionAccessGetter<GenericElectionAccess> {
	type ElectoralSystem: ElectoralSystem;
	type ElectionAccess<'a>: MultiElectionWriteAccess<Self::ElectoralSystem>
	where
		Self: 'a,
		GenericElectionAccess: 'a;

	fn get_election_access<'a>(
		generic_election_access: &'a mut GenericElectionAccess,
	) -> Self::ElectionAccess<'a>;
}

macro_rules! generate_electoral_system_tuple_impls {
    ($module:ident: ($(($t:ident, $t_alt:ident)),*$(,)?) : ($($t_dup:ident),*$(,)?)) => {
        mod $module {
            use super::{
                super::{
                    ElectoralSystem,
                    ConsensusStatus,
                    ElectionReadAccess,
                    ElectionWriteAccess,
                    MultiElectionReadAccess,
                    MultiElectionWriteAccess
                },
                ElectionAccessGetter,
                ElectoralSystemTuple,
                Composite
            };

            use crate::{
                CorruptStorageError,
                vote_storage::{
                    PartialOrVote,
                    VoteStorage
                },
                ElectionIdentifier
            };
            use crate::vote_storage::composite::$module::CompositeVoteStorageEnum;

            use cf_primitives::AuthorityCount;

            use codec::{Encode, Decode};
            use scale_info::TypeInfo;

            /// This trait specifies the behaviour of the composite's `ElectoralSystem::on_finalize` without that coding being exposed to the internals of the composite by using the ElectionAccessGetter trait to obtain ElectionAccess objects that abstract those details.
            pub trait OnFinalize<$($t: ElectoralSystem,)*>: 'static {
                /// The `Context` for the composite's ElectoralSystem implementation.
                type Context;

                fn on_finalize<GenericElectionAccess, $($t_alt: ElectionAccessGetter<GenericElectionAccess, ElectoralSystem = $t>),*>(
                    election_identifiers: ($(Vec<ElectionIdentifier<$t::ElectionIdentifierExtra>>,)*),
                    generic_election_access: &mut GenericElectionAccess,
                    context: &Self::Context,
                ) -> Result<(), CorruptStorageError>;
            }

            #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Encode, Decode, TypeInfo)]
            pub enum CompositeElectoralSystemEnum<$($t,)*> {
                $($t($t),)*
            }

            // In the 1/identity case, no invalid combinations are possible, so error cases are unreachable.
            #[allow(unreachable_patterns)]
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            impl<S: OnFinalize<$($t),*>, $($t: ElectoralSystem),*> ElectoralSystem for Composite<S, ($($t,)*)> {
                type ElectoralUnsynchronisedState = ($(<$t as ElectoralSystem>::ElectoralUnsynchronisedState,)*);
                type ElectoralUnsynchronisedSettings = ($(<$t as ElectoralSystem>::ElectoralUnsynchronisedSettings,)*);
                type ElectoralSettings = ($(<$t as ElectoralSystem>::ElectoralSettings,)*);

                type ElectionIdentifierExtra = CompositeElectoralSystemEnum<$(<$t as ElectoralSystem>::ElectionIdentifierExtra,)*>;
                type ElectionProperties = CompositeElectoralSystemEnum<$(<$t as ElectoralSystem>::ElectionProperties,)*>;
                type ElectionState = CompositeElectoralSystemEnum<$(<$t as ElectoralSystem>::ElectionState,)*>;
                type Vote = ($(<$t as ElectoralSystem>::Vote,)*);
                type Consensus = CompositeElectoralSystemEnum<$(<$t as ElectoralSystem>::Consensus,)*>;

                type Context = S::Context;

                fn is_vote_desired<ElectionAccess: ElectionReadAccess<Self>>(
                    election_identifier: ElectionIdentifier<Self::ElectionIdentifierExtra>,
                    election_access: &ElectionAccess,
                    current_vote: Option<(
                        <Self::Vote as VoteStorage>::Properties,
                        PartialOrVote<
                            <Self::Vote as VoteStorage>::PartialVote,
                            <Self::Vote as VoteStorage>::Vote,
                        >,
                    )>,
                ) -> Result<Option<core::time::Duration>, CorruptStorageError> {
                    match *election_identifier.extra() {
                        $(CompositeElectoralSystemEnum::$t(extra) => {
                            <$t as ElectoralSystem>::is_vote_desired(
                                ElectionIdentifier::new(*election_identifier.unique_monotonic(), extra),
                                &CompositeElectionAccessRef::new(election_access),
                                current_vote.map(|(properties, vote)| {
                                    Ok((
                                        match properties {
                                            CompositeVoteStorageEnum::$t(properties) => properties,
                                            _ => return Err(CorruptStorageError),
                                        },
                                        match vote {
                                            PartialOrVote::PartialVote(CompositeVoteStorageEnum::$t(partial_vote)) => PartialOrVote::PartialVote(partial_vote),
                                            PartialOrVote::Vote(CompositeVoteStorageEnum::$t(vote)) => PartialOrVote::Vote(vote),
                                            _ => return Err(CorruptStorageError),
                                        },
                                    ))
                                }).transpose()?,
                            )
                        },)*
                    }
                }

                fn is_vote_valid<ElectionAccess: ElectionReadAccess<Self>>(
                    election_identifier: ElectionIdentifier<Self::ElectionIdentifierExtra>,
                    election_access: &ElectionAccess,
                    partial_vote: <Self::Vote as VoteStorage>::PartialVote,
                ) -> Result<bool, CorruptStorageError> {
                    Ok(match (*election_identifier.extra(), partial_vote) {
                        $((CompositeElectoralSystemEnum::$t(extra), CompositeVoteStorageEnum::$t(partial_vote)) => <$t as ElectoralSystem>::is_vote_valid(
                            ElectionIdentifier::new(*election_identifier.unique_monotonic(), extra),
                            &CompositeElectionAccessRef::new(election_access),
                            partial_vote,
                        )?,)*
                        _ => false,
                    })
                }

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
                ) -> Result<<Self::Vote as VoteStorage>::Properties, CorruptStorageError> {
                    match (*election_identifier.extra(), vote) {
                        $((CompositeElectoralSystemEnum::$t(extra), CompositeVoteStorageEnum::$t(vote)) => {
                            <$t as ElectoralSystem>::vote_properties(
                                ElectionIdentifier::new(*election_identifier.unique_monotonic(), extra),
                                previous_vote.map(|(previous_properties, previous_vote)| {
                                    Ok((
                                        match previous_properties {
                                            CompositeVoteStorageEnum::$t(previous_properties) => previous_properties,
                                            _ => return Err(CorruptStorageError),
                                        },
                                        match previous_vote {
                                            PartialOrVote::PartialVote(CompositeVoteStorageEnum::$t(partial_vote)) => PartialOrVote::PartialVote(partial_vote),
                                            PartialOrVote::Vote(CompositeVoteStorageEnum::$t(vote)) => PartialOrVote::Vote(vote),
                                            _ => return Err(CorruptStorageError),
                                        },
                                    ))
                                }).transpose()?,
                                vote,
                            ).map(CompositeVoteStorageEnum::$t)
                        },)*
                        _ => Err(CorruptStorageError),
                    }
                }

                #[allow(non_snake_case)]
                fn on_finalize<MultiElectionAccess: MultiElectionWriteAccess<Self>>(
                    election_identifiers: Vec<ElectionIdentifier<Self::ElectionIdentifierExtra>>,
                    election_access: &mut MultiElectionAccess,
                    context: &Self::Context,
                ) -> Result<(), CorruptStorageError> {
                    $(let mut $t_alt = Vec::new();)*

                    for election_identifier in election_identifiers {
                        match *election_identifier.extra() {
                            $(CompositeElectoralSystemEnum::$t(extra) => {
                                $t_alt.push(ElectionIdentifier::new(*election_identifier.unique_monotonic(), extra));
                            })*
                        }
                    }

                    S::on_finalize::<
                        MultiElectionAccess,
                        $(ConcreteElectionAccessTranslator<S, tags::$t, <Self as ElectoralSystemTuple>::Type>),*
                    >(
                        ($($t_alt,)*),
                        election_access,
                        context,
                    )
                }

                fn check_consensus<ElectionAccess: ElectionReadAccess<Self>>(
                    election_identifier: ElectionIdentifier<Self::ElectionIdentifierExtra>,
                    election_access: &ElectionAccess,
                    previous_consensus: Option<&Self::Consensus>,
                    votes: Vec<(<Self::Vote as VoteStorage>::Properties, <Self::Vote as VoteStorage>::Vote)>,
                    authorities: AuthorityCount,
                ) -> Result<Option<Self::Consensus>, CorruptStorageError> {
                    Ok(match *election_identifier.extra() {
                        $(CompositeElectoralSystemEnum::$t(extra) => {
                            <$t as ElectoralSystem>::check_consensus(
                                ElectionIdentifier::new(*election_identifier.unique_monotonic(), extra),
                                &CompositeElectionAccessRef::new(election_access),
                                previous_consensus.map(|previous_consensus| {
                                    match previous_consensus {
                                        CompositeElectoralSystemEnum::$t(previous_consensus) => Ok(previous_consensus),
                                        _ => Err(CorruptStorageError),
                                    }
                                }).transpose()?,
                                votes.into_iter().map(|(properties, vote)| {
                                    match (properties, vote) {
                                        (
                                            CompositeVoteStorageEnum::$t(properties),
                                            CompositeVoteStorageEnum::$t(vote)
                                        ) => Ok((properties, vote)),
                                        _ => Err(CorruptStorageError),
                                    }
                                }).collect::<Result<Vec<_>, _>>()?,
                                authorities,
                            )?.map(CompositeElectoralSystemEnum::$t)
                        },)*
                    })
                }
            }

            mod tags {
                $(
                    /// The wrappers need to impl the access traits once for each variant,
                    /// these tags ensure these trait impls don't overlap.
                    #[allow(dead_code)]
                    pub(super) struct $t;
                )*
            }

            struct CompositeElectionAccessRef<'a, S, Tag, $($t: ElectoralSystem,)* EA> {
                ea: &'a EA,
                _phantom: core::marker::PhantomData<(S, Tag, $($t,)*)>,
            }
            impl<'a, S: OnFinalize<$($t),*>, Tag, $($t: ElectoralSystem,)* EA: ElectionReadAccess<Composite<S, ($($t,)*)>>> CompositeElectionAccessRef<'a, S, Tag, $($t),*, EA> {
                fn new(ea: &'a EA) -> Self {
                    Self {
                        ea,
                        _phantom: Default::default(),
                    }
                }
            }

            struct CompositeElectionAccess<S, Tag, $($t: ElectoralSystem,)* EA> {
                ea: EA,
                _phantom: core::marker::PhantomData<(S, Tag, $($t,)*)>,
            }
            impl<S: OnFinalize<$($t),*>, Tag, $($t: ElectoralSystem,)* EA: ElectionReadAccess<Composite<S, ($($t,)*)>>> CompositeElectionAccess<S, Tag, $($t),*, EA> {
                fn new(ea: EA) -> Self {
                    Self {
                        ea,
                        _phantom: Default::default(),
                    }
                }
            }
            struct CompositeMultiElectionAccess<'a, S: OnFinalize<$($t),*>, Tag, $($t: ElectoralSystem,)* MEA: MultiElectionWriteAccess<Composite<S, ($($t,)*)>>> {
                mea: &'a mut MEA,
                _phantom: core::marker::PhantomData<(S, Tag, $($t,)*)>,
            }
            impl<'b, S: OnFinalize<$($t),*>, Tag, $($t: ElectoralSystem,)* MEA: MultiElectionWriteAccess<Composite<S, ($($t,)*)>>> CompositeMultiElectionAccess<'b, S, Tag, $($t),*, MEA> {
                fn new(mea: &'b mut MEA) -> Self {
                    Self {
                        mea,
                        _phantom: Default::default(),
                    }
                }
            }

            struct ConcreteElectionAccessTranslator<S, Tag, T> {
                _phantom: core::marker::PhantomData<(S, Tag, T)>,
            }

            // This macro solves the problem of taking a repeating argument and generating the
            // product of the arguments elements. As we need to be able to refer to every element
            // individually, while also referencing to the whole list.
            generate_electoral_system_tuple_impls!(@;$($t,)*:$($t,)*);
        }
    };
    (@ $($previous:ident,)*;: $($t:ident,)*) => {};
    (@ $($previous:ident,)*; $i:ident, $($remaining:ident,)*: $($t:ident,)*) => {
        // In the 1/identity case, no invalid combinations are possible, so error cases are unreachable.
        #[allow(unreachable_patterns)]
        impl<'a, S: OnFinalize<$($t),*>, $($t: ElectoralSystem,)* EA: ElectionReadAccess<Composite<S, ($($t,)*)>>> ElectionReadAccess<$i> for CompositeElectionAccessRef<'a, S, tags::$i, $($t),*, EA> {
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            fn settings(&self) -> Result<$i::ElectoralSettings, CorruptStorageError> {
                let ($($previous,)* settings, $($remaining,)*) =self.ea.settings()?;
                Ok(settings)
            }
            fn properties(&self) -> Result<$i::ElectionProperties, CorruptStorageError> {
                match self.ea.properties()? {
                    CompositeElectoralSystemEnum::$i(properties) => {
                        Ok(properties)
                    },
                    _ => Err(CorruptStorageError)
                }
            }
            fn state(&self) -> Result<$i::ElectionState, CorruptStorageError> {
                match self.ea.state()? {
                    CompositeElectoralSystemEnum::$i(state) => {
                        Ok(state)
                    },
                    _ => Err(CorruptStorageError)
                }
            }
        }
        // In the 1/identity case, no invalid combinations are possible, so error cases are unreachable.
        #[allow(unreachable_patterns)]
        impl<S: OnFinalize<$($t),*>, $($t: ElectoralSystem,)* EA: ElectionReadAccess<Composite<S, ($($t,)*)>>> ElectionReadAccess<$i> for CompositeElectionAccess<S, tags::$i, $($t),*, EA> {
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            fn settings(&self) -> Result<$i::ElectoralSettings, CorruptStorageError> {
                let ($($previous,)* settings, $($remaining,)*) =self.ea.settings()?;
                Ok(settings)
            }
            fn properties(&self) -> Result<$i::ElectionProperties, CorruptStorageError> {
                match self.ea.properties()? {
                    CompositeElectoralSystemEnum::$i(properties) => {
                        Ok(properties)
                    },
                    _ => Err(CorruptStorageError)
                }
            }
            fn state(&self) -> Result<$i::ElectionState, CorruptStorageError> {
                match self.ea.state()? {
                    CompositeElectoralSystemEnum::$i(state) => {
                        Ok(state)
                    },
                    _ => Err(CorruptStorageError)
                }
            }
        }
        impl<S: OnFinalize<$($t),*>, $($t: ElectoralSystem,)* EA: ElectionWriteAccess<Composite<S, ($($t,)*)>>> ElectionWriteAccess<$i> for CompositeElectionAccess<S, tags::$i, $($t),*, EA> {
            fn set_state(&mut self, state: $i::ElectionState) -> Result<(), CorruptStorageError> {
                self.ea.set_state(CompositeElectoralSystemEnum::$i(state))
            }
            fn clear_votes(&mut self) {
                self.ea.clear_votes()
            }
            fn delete(self) {
                self.ea.delete();
            }
            // In the 1/identity case, no invalid combinations are possible, so error cases are unreachable.
            #[allow(unreachable_patterns)]
            fn check_consensus(
                &mut self,
            ) -> Result<ConsensusStatus<$i::Consensus>, CorruptStorageError> {
                self.ea.check_consensus().and_then(|consensus_status| {
                    consensus_status.try_map(|consensus| {
                        match consensus {
                            CompositeElectoralSystemEnum::$i(consensus) => Ok(consensus),
                            _ => Err(CorruptStorageError),
                        }
                    })

                })
            }
        }
        impl<'b, S: OnFinalize<$($t),*>, $($t: ElectoralSystem,)* MEA: MultiElectionWriteAccess<Composite<S, ($($t,)*)>>> MultiElectionReadAccess<$i> for CompositeMultiElectionAccess<'b, S, tags::$i, $($t),*, MEA> {
            type ElectionReadAccess<'a> = CompositeElectionAccess<S, tags::$i, $($t),*, <MEA as MultiElectionReadAccess<Composite<S, ($($t,)*)>>>::ElectionReadAccess<'a>>
            where
                Self: 'a;

            fn election(
                &self,
                id: ElectionIdentifier<<$i as ElectoralSystem>::ElectionIdentifierExtra>,
            ) -> Result<Self::ElectionReadAccess<'_>, CorruptStorageError> {
                self.mea.election(ElectionIdentifier::new(*id.unique_monotonic(), CompositeElectoralSystemEnum::$i(*id.extra()))).map(|election_access| {
                    CompositeElectionAccess::<S, tags::$i, $($t),*, _>::new(election_access)
                })
            }
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            fn unsynchronised_settings(
                &self,
            ) -> Result<$i::ElectoralUnsynchronisedSettings, CorruptStorageError> {
                let ($($previous,)* unsynchronised_settings, $($remaining,)*) =self.mea.unsynchronised_settings()?;
                Ok(unsynchronised_settings)
            }
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            fn unsynchronised_state(
                &self,
            ) -> Result<$i::ElectoralUnsynchronisedState, CorruptStorageError> {
                let ($($previous,)* unsynchronised_state, $($remaining,)*) =self.mea.unsynchronised_state()?;
                Ok(unsynchronised_state)
            }
        }

        impl<'b, S: OnFinalize<$($t),*>, $($t: ElectoralSystem,)* MEA: MultiElectionWriteAccess<Composite<S, ($($t,)*)>>> MultiElectionWriteAccess<$i> for CompositeMultiElectionAccess<'b, S, tags::$i, $($t),*, MEA> {
            type ElectionWriteAccess<'a> = CompositeElectionAccess<S, tags::$i, $($t),*, <MEA as MultiElectionWriteAccess<Composite<S, ($($t,)*)>>>::ElectionWriteAccess<'a>>
            where
                Self: 'a;

            fn new_election(
                &mut self,
                extra: $i::ElectionIdentifierExtra,
                properties: $i::ElectionProperties,
                state: $i::ElectionState,
            ) -> Result<Self::ElectionWriteAccess<'_>, CorruptStorageError> {
                self.mea.new_election(CompositeElectoralSystemEnum::$i(extra), CompositeElectoralSystemEnum::$i(properties), CompositeElectoralSystemEnum::$i(state)).map(|election_access| {
                    CompositeElectionAccess::<S, tags::$i, $($t),*, _>::new(election_access)
                })
            }
            fn election_mut(
                &mut self,
                id: ElectionIdentifier<$i::ElectionIdentifierExtra>,
            ) -> Result<Self::ElectionWriteAccess<'_>, CorruptStorageError> {
                self.mea.election_mut(ElectionIdentifier::new(*id.unique_monotonic(), CompositeElectoralSystemEnum::$i(*id.extra()))).map(|election_access| {
                    CompositeElectionAccess::<S, tags::$i, $($t),*, _>::new(election_access)
                })
            }
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            fn set_unsynchronised_state(
                &mut self,
                unsynchronised_state: $i::ElectoralUnsynchronisedState,
            ) -> Result<(), CorruptStorageError> {
                let ($($previous,)* _, $($remaining,)*) = self.mea.unsynchronised_state()?;
                self.mea.set_unsynchronised_state(($($previous,)* unsynchronised_state, $($remaining,)*))
            }
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            fn mutate_unsynchronised_state<
                T,
                F: for<'a> FnOnce(
                    &mut Self,
                    &'a mut $i::ElectoralUnsynchronisedState,
                ) -> Result<T, CorruptStorageError>,
            >(
                &mut self,
                f: F,
            ) -> Result<T, CorruptStorageError> {
                let ($($previous,)* mut unsynchronised_state, $($remaining,)*) = self.mea.unsynchronised_state()?;
                let t = f(self, &mut unsynchronised_state)?;
                self.mea.set_unsynchronised_state(($($previous,)* unsynchronised_state, $($remaining,)*))?;
                Ok(t)
            }
        }

        impl<S: OnFinalize<$($t),*>, $($t: ElectoralSystem,)* MEA: MultiElectionWriteAccess<Composite<S, ($($t,)*)>>> ElectionAccessGetter<MEA> for ConcreteElectionAccessTranslator<S, tags::$i, ($($t,)*)> {
            type ElectoralSystem = $i;
            type ElectionAccess<'a> = CompositeMultiElectionAccess<'a, S, tags::$i, $($t),*, MEA>
            where
                Self: 'a, MEA: 'a;

            fn get_election_access<'a>(generic_election_access: &'a mut MEA) -> Self::ElectionAccess<'a> {
                Self::ElectionAccess::<'a>::new(generic_election_access)
            }
        }

        generate_electoral_system_tuple_impls!(@ $($previous,)* $i,; $($remaining,)*: $($t,)*);
    };
}

generate_electoral_system_tuple_impls!(tuple_1_impls: ((A, A0)) : (A));
generate_electoral_system_tuple_impls!(tuple_2_impls: ((A, A0), (B, B0)): (A, B));
generate_electoral_system_tuple_impls!(tuple_3_impls: ((A, A0), (B, B0), (C, C0)): (A, B, C));
generate_electoral_system_tuple_impls!(tuple_4_impls: ((A, A0), (B, B0), (C, C0), (D, D0)): (A, B, C, D));
