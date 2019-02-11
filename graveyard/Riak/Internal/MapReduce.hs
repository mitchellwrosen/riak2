-- | Map an input to itself.
riakMapReducePhaseMapIdentity :: RiakMapReducePhase
riakMapReducePhaseMapIdentity =
  RiakMapReducePhaseMap "riak_kv_mapreduce" "map_identity"

-- | Map an object to its value.
riakMapReducePhaseMapObjectValue :: RiakMapReducePhase
riakMapReducePhaseMapObjectValue =
  RiakMapReducePhaseMap "riak_kv_mapreduce" "map_object_value"

-- | Reduce inputs to their count.
riakMapReducePhaseReduceCount :: RiakMapReducePhase
riakMapReducePhaseReduceCount =
  RiakMapReducePhaseReduce "riak_kv_mapreduce" "reduce_count_inputs"

-- | Reduce inputs to their union.
riakMapReducePhaseReduceSetUnion :: RiakMapReducePhase
riakMapReducePhaseReduceSetUnion =
  RiakMapReducePhaseReduce "riak_kv_mapreduce" "reduce_set_union"

-- | Reduce inputs to their sorted values.
riakMapReducePhaseReduceSort :: RiakMapReducePhase
riakMapReducePhaseReduceSort =
  RiakMapReducePhaseReduce "riak_kv_mapreduce" "reduce_sort"

-- | Reduce inputs to their sum.
riakMapReducePhaseReduceSum :: RiakMapReducePhase
riakMapReducePhaseReduceSum =
  RiakMapReducePhaseReduce "riak_kv_mapreduce" "reduce_sum"
