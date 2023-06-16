module Node.Http2.Headers where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Http2.Types (Headers)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

-- | Makes an `Headers` value where all headers are insensitive.
mkHeadersI :: forall insensitive. { | insensitive } -> Headers
mkHeadersI = unsafeCoerce

-- | Makes an `Headers` value containing both insensitive headers (first argument)
-- | and sensitive headers (second argument).
-- | ```
-- | mkHeaders { ":method": "GET" } { "sensitive-header": "secret" }
-- | ```
-- |
-- | Note: the `Union` and `Nub` constraints simply prove that labels in one record
-- | do not appear in the other one.
mkHeaders
  :: forall insensitive sensitive all
   . Row.Union insensitive sensitive all
  => Row.Nub all all
  => { | insensitive }
  -> { | sensitive }
  -> Headers
mkHeaders insensitive sensitive =
  runFn3 mkHeadersImpl insensitive sensitive (Object.keys $ unsafeCoerce sensitive)

foreign import mkHeadersImpl :: forall insensitive sensitive. Fn3 { | insensitive } { | sensitive } (Array String) (Headers)

-- | Note: this is unsafe because 
-- | - a server response's ":status" pseudo header's value will have type `Int`, not `String`
-- | - a server response's "set-cookie" header's value will have type `Array String`, not `String`
-- | 
-- | See https://nodejs.org/dist/latest-v18.x/docs/api/http2.html#headers-object
unsafeToObject :: Headers -> Object String
unsafeToObject = unsafeCoerce

status :: Headers -> Maybe Int
status = unsafeCoerce <<< lookup ":status"

method :: Headers -> Maybe String
method = lookup ":method"

scheme :: Headers -> Maybe String
scheme = lookup ":scheme"

authority :: Headers -> Maybe String
authority = lookup ":authority"

path :: Headers -> Maybe String
path = lookup ":path"

lookup :: String -> Headers -> Maybe String
lookup key headers = Object.lookup key $ unsafeCoerce headers

printHeaders :: Headers -> String
printHeaders = printHeaders' "; "

printHeaders' :: String -> Headers -> String
printHeaders' sep r = _.val $ foldlWithIndex printKeyValue { init: true, val: "" } $ (unsafeCoerce r :: Object String)
  where
  printKeyValue k acc v = do
    let kv = k <> "=" <> v
    { init: false
    , val: if acc.init then kv else acc.val <> sep <> kv
    }
