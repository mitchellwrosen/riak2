module RiakMapReduceResult where


import RiakErlangTerm (ErlangTerm)

data MapReduceResult
  = MapReduceResult
  { phase :: Natural
  , result :: ErlangTerm
  } deriving stock (Generic, Show)
