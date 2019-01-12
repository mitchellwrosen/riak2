import Data.ProtoLens.Setup
import Distribution.Simple

main :: IO ()
main =
  defaultMainWithHooks userHooks

userHooks :: UserHooks
userHooks =
  generatingSpecificProtos
    "."
    (\_ -> pure ["riak.proto"])
    simpleUserHooks
