import Data.ProtoLens.Setup
import Distribution.Simple

main :: IO ()
main =
  defaultMainWithHooks
    ((generatingSpecificProtos
      "."
      (\_ -> pure ["riak.proto"])
      simpleUserHooks)
    { sDistHook = sDistHook simpleUserHooks })


