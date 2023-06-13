module Node.Http2.Server
  ( onCheckContinue
  , onConnection
  , onRequest
  , onSession
  , onSessionError
  , onStream
  , onTimeout
  , onUnknownProtocol
  , close
  , setTimeout
  , timeout
  , updateSettings
  ) where

import Prelude

import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, mkEffectFn1, mkEffectFn2, mkEffectFn4, runEffectFn1, runEffectFn2)
import Node.Http2.Types (Headers, Http2SecureServer, Http2ServerRequest, Http2ServerResponse, Http2Session, Http2Stream, Server, Settings)
import Node.Stream (Duplex)

onCheckContinue :: Http2SecureServer -> (Http2ServerRequest -> Http2ServerResponse -> Effect Unit) -> Effect Unit
onCheckContinue s cb = runEffectFn2 onCheckContinueImpl s $ mkEffectFn2 cb

foreign import onCheckContinueImpl :: EffectFn2 (Http2SecureServer) (EffectFn2 Http2ServerRequest Http2ServerResponse Unit) (Unit)

onConnection :: Http2SecureServer -> (Duplex -> Effect Unit) -> Effect Unit
onConnection s cb = runEffectFn2 onConnectionImpl s $ mkEffectFn1 cb

foreign import onConnectionImpl :: EffectFn2 (Http2SecureServer) (EffectFn1 Duplex Unit) (Unit)

onRequest :: Http2SecureServer -> (Http2ServerRequest -> Http2ServerResponse -> Effect Unit) -> Effect Unit
onRequest s cb = runEffectFn2 onRequestImpl s $ mkEffectFn2 cb

foreign import onRequestImpl :: EffectFn2 (Http2SecureServer) (EffectFn2 Http2ServerRequest Http2ServerResponse Unit) (Unit)

onSession :: Http2SecureServer -> (Http2Session Server -> Effect Unit) -> Effect Unit
onSession s cb = runEffectFn2 onSessionImpl s $ mkEffectFn1 cb

foreign import onSessionImpl :: EffectFn2 (Http2SecureServer) (EffectFn1 (Http2Session Server) Unit) (Unit)

onSessionError :: Http2SecureServer -> (Error -> Http2Session Server -> Effect Unit) -> Effect Unit
onSessionError s cb = runEffectFn2 onSessionErrorImpl s $ mkEffectFn2 cb

foreign import onSessionErrorImpl :: EffectFn2 (Http2SecureServer) (EffectFn2 Error (Http2Session Server) Unit) (Unit)

onStream :: Http2SecureServer -> (Http2Stream Server -> Headers -> Int -> Array String -> Effect Unit) -> Effect Unit
onStream s cb = runEffectFn2 onStreamImpl s $ mkEffectFn4 cb

foreign import onStreamImpl :: EffectFn2 (Http2SecureServer) (EffectFn4 (Http2Stream Server) Headers Int (Array String) Unit) (Unit)

onTimeout :: Http2SecureServer -> Effect Unit -> Effect Unit
onTimeout s cb = runEffectFn2 onTimeoutImpl s cb

foreign import onTimeoutImpl :: EffectFn2 (Http2SecureServer) (Effect Unit) (Unit)

onUnknownProtocol :: Http2SecureServer -> (Duplex -> Effect Unit) -> Effect Unit
onUnknownProtocol s cb = runEffectFn2 onUnknownProtocolImpl s $ mkEffectFn1 cb

foreign import onUnknownProtocolImpl :: EffectFn2 (Http2SecureServer) (EffectFn1 Duplex Unit) (Unit)

close :: Http2SecureServer -> Effect Unit
close s = runEffectFn1 closeImpl s

foreign import closeImpl :: EffectFn1 (Http2SecureServer) (Unit)

setTimeout :: Http2SecureServer -> Milliseconds -> Effect Http2SecureServer
setTimeout s ms = runEffectFn2 setTimeoutImpl s ms

foreign import setTimeoutImpl :: EffectFn2 (Http2SecureServer) (Milliseconds) (Http2SecureServer)

timeout :: Http2SecureServer -> Effect Milliseconds
timeout s = runEffectFn1 timeoutImpl s

foreign import timeoutImpl :: EffectFn1 (Http2SecureServer) (Milliseconds)

updateSettings :: Http2SecureServer -> Settings -> Effect Unit
updateSettings s set = runEffectFn2 updateSettingsImpl s set

foreign import updateSettingsImpl :: EffectFn2 (Http2SecureServer) (Settings) (Unit)

