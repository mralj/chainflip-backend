use super::*;
use crate::{
	vote_storage::{SharedDataHash, VoteStorage},
	CorruptStorageError,
};

#[test]
fn test_simple_vote_storage() {
	type SimpleStorageTest = Simple<u8, (individual::Individual<u64>, shared::Shared<u64>)>;

	let test_properties = 7u8;
	let test_individual_vote = 19u64;
	let test_shared_vote = 67u64;
	let test_shared_vote_data = <SimpleStorageTest as VoteStorage>::SharedData::B(test_shared_vote);
	let test_shared_data_hash = SharedDataHash::of(&test_shared_vote_data);
	let test_vote: <SimpleStorageTest as VoteStorage>::Vote =
		(test_individual_vote, test_shared_vote);

	let partial_vote = SimpleStorageTest::vote_into_partial_vote(
		&test_vote,
		|shared_data: <SimpleStorageTest as VoteStorage>::SharedData| {
			SharedDataHash::of(&shared_data)
		},
	);
	assert_eq!(partial_vote, (test_individual_vote, test_shared_data_hash),);

	let vote_components =
		SimpleStorageTest::partial_vote_into_components(test_properties, partial_vote);
	assert_eq!(vote_components.individual_component, Some((test_properties, partial_vote)));
	assert_eq!(vote_components.bitmap_component, None);

	let vote = SimpleStorageTest::components_into_vote(
		VoteComponents {
			individual_component: Some((test_properties, partial_vote)),
			bitmap_component: None,
		},
		|shared_data_hash| {
			assert_eq!(shared_data_hash, test_shared_data_hash);
			Some(test_shared_vote_data.clone())
		},
	);
	assert_eq!(vote, Some((test_properties, PartialOrVote::Vote(test_vote.clone()),)));

	let vote = SimpleStorageTest::components_into_vote(
		VoteComponents {
			individual_component: Some((test_properties, partial_vote)),
			bitmap_component: None,
		},
		|shared_data_hash| {
			assert_eq!(shared_data_hash, test_shared_data_hash);
			None
		},
	);
	assert_eq!(vote, Some((test_properties, PartialOrVote::PartialVote(partial_vote.clone()),)));

	let vote = SimpleStorageTest::components_into_vote(
		VoteComponents { individual_component: None, bitmap_component: None },
		|_shared_data_hash| {
			assert!(false);
			None
		},
	);
	assert_eq!(vote, None);

	let vote = SimpleStorageTest::components_into_vote(
		VoteComponents {
			individual_component: None,
			bitmap_component: Some(test_shared_data_hash),
		},
		|_shared_data_hash| {
			assert!(false);
			None
		},
	);
	assert_eq!(vote, None);

	assert_eq!(
		SimpleStorageTest::visit_vote(
			test_vote.clone(),
			|shared_data: <SimpleStorageTest as VoteStorage>::SharedData| {
				assert_eq!(shared_data, test_shared_vote_data);
				assert_eq!(SharedDataHash::of(&shared_data), test_shared_data_hash);
				Ok::<_, CorruptStorageError>(())
			}
		),
		Ok(())
	);

	SimpleStorageTest::visit_individual_component(&partial_vote, |shared_data_hash| {
		assert_eq!(shared_data_hash, test_shared_data_hash);
	});
}
