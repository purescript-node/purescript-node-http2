module Node.Http2.Server
  ( toTlsServer
  , createSecureServer
  , checkContinueH
  , requestH
  , sessionH
  , sessionErrorH
  , streamH
  , timeoutH
  , unknownProtocolH
  , setTimeout
  , timeout
  , updateSettings
  ) where

import Prelude

import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, mkEffectFn4, runEffectFn1, runEffectFn2)
import Node.EventEmitter (EventHandle(..))
import Node.EventEmitter.UtilTypes (EventHandle1, EventHandle2, EventHandle4, EventHandle0)
import Node.Http2.Flags (BitwiseFlag)
import Node.Http2.Types (Headers, Http2CreateSecureServerOptions, Http2SecureServer, Http2ServerRequest, Http2ServerResponse, Http2Session, Http2Stream, Settings)
import Node.Net.Types (NewServerOptions)
import Node.Stream (Duplex)
import Node.TLS.Types (CreateSecureContextOptions, Server, TlsCreateServerOptions, TlsServer)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

toTlsServer :: Http2SecureServer -> TlsServer
toTlsServer = unsafeCoerce

createSecureServer
  :: forall rec trash
   . Row.Union rec trash (Http2CreateSecureServerOptions (TlsCreateServerOptions Server (CreateSecureContextOptions (NewServerOptions ()))))
  => { | rec }
  -> Effect Http2SecureServer
createSecureServer options = runEffectFn1 createSecureServerImpl options

foreign import createSecureServerImpl :: forall r. EffectFn1 { | r } (Http2SecureServer)

checkContinueH :: EventHandle2 Http2SecureServer Http2ServerRequest Http2ServerResponse
checkContinueH = EventHandle "checkContinue" \cb -> mkEffectFn2 \a b -> cb a b

requestH :: EventHandle2 Http2SecureServer Http2ServerRequest Http2ServerResponse
requestH = EventHandle "request" \cb -> mkEffectFn2 \a b -> cb a b

sessionH :: EventHandle1 Http2SecureServer (Http2Session Server)
sessionH = EventHandle "session" mkEffectFn1

sessionErrorH :: EventHandle2 Http2SecureServer Error (Http2Session Server)
sessionErrorH = EventHandle "sessionError" \cb -> mkEffectFn2 \a b -> cb a b

streamH :: EventHandle4 Http2SecureServer (Http2Stream Server) (Headers) BitwiseFlag (Array String)
streamH = EventHandle "stream" \cb -> mkEffectFn4 \a b c d -> cb a b c d

timeoutH :: EventHandle0 TlsServer
timeoutH = EventHandle "timeout" identity

unknownProtocolH :: EventHandle1 Http2SecureServer Duplex
unknownProtocolH = EventHandle "unknownProtocol" mkEffectFn1

setTimeout :: Http2SecureServer -> Milliseconds -> Effect Http2SecureServer
setTimeout s ms = runEffectFn2 setTimeoutImpl s ms

foreign import setTimeoutImpl :: EffectFn2 (Http2SecureServer) (Milliseconds) (Http2SecureServer)

timeout :: Http2SecureServer -> Effect Milliseconds
timeout s = runEffectFn1 timeoutImpl s

foreign import timeoutImpl :: EffectFn1 (Http2SecureServer) (Milliseconds)

updateSettings :: Http2SecureServer -> Settings -> Effect Unit
updateSettings s set = runEffectFn2 updateSettingsImpl s set

foreign import updateSettingsImpl :: EffectFn2 (Http2SecureServer) (Settings) (Unit)

