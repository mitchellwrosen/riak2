-- |
-- * <https://docs.basho.com/riak/kv/2.2.3/developing/usage/search/>
-- * <https://docs.basho.com/riak/kv/2.2.3/developing/usage/searching-data-types/>
--
-- CRDT search examples:
--
-- * Counters
--
--     The default Solr schema indexes counters under the field name
--     @"counter"@.
--
--     For example, to search for counters in bucket @"foo"@ with values greater
--     than 100:
--
--     > _yz_rb:foo AND counter:{100 TO *]
--
-- * Sets
--
-- * Maps
--
--     The default Solr schema indexes dynamic fields ending in @"_counter"@,
--     @"_flag"@, @"_register"@, @"_set"@, and the default
--     @application/riak_map@ extractor dynamically renames map keys per their
--     type by suffixing them with @"_counter"@, @"_flag"@, @"_map"@,
--     @"_register"@, or @"_set@", using @'.'@ as a delimiter for nested maps.
--
--     For example, to search for maps with an inner map named @"foo"@ that
--     contain an inner map named @"bar"@ that contain a set named @"baz"@ that
--     contain the string @"qux"@:
--
--     > foo_map.bar_map.baz_set:qux

module Riak.Search
  ( search
  , SearchOpts(..)
  , SearchResults(..)
  ) where

import RiakSearch
