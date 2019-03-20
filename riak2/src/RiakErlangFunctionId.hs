module RiakErlangFunctionId where


-- | An Erlang function identifier (module name and function name).
data ErlangFunctionId
  = ErlangFunctionId Text Text
  deriving stock (Show)
