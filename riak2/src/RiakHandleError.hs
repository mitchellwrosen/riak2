module RiakHandleError where


data HandleError :: Type where
  -- | A request timed out waiting for a handle to become connected.
  HandleTimeoutError :: HandleError
  -- | A request was attempted the maximum number of times.
  HandleRetryError :: HandleError
  deriving stock (Eq, Show)
