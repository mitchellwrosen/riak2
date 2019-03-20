module RiakErlangFunction where


-- | An Erlang function (module name and function name).
data ErlangFunction
  = ErlangFunction Text Text
  deriving stock (Show)
