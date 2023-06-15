module Node.Http2.Client
  ( connect
  , connect'
  ) where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Node.Http2.Types (Http2ClientConnectOptions, Http2Session)
import Node.Net.Types (ConnectTcpOptions)
import Node.TLS.Types (Client, ConnectTlsSocketOptions, CreateSecureContextOptions)
import Prim.Row as Row

connect :: String -> Effect (Http2Session Client)
connect authority = runEffectFn1 connectAuthImpl authority

foreign import connectAuthImpl :: EffectFn1 (String) (Http2Session Client)

connect'
  :: forall rec trash
   . Row.Union rec trash (Http2ClientConnectOptions (ConnectTlsSocketOptions Client (CreateSecureContextOptions (ConnectTcpOptions ()))))
  => String
  -> { | rec }
  -> Effect (Http2Session Client)
connect' authority rec = runEffectFn2 connectAuthOptionsImpl authority rec

foreign import connectAuthOptionsImpl :: forall r. EffectFn2 (String) ({ | r }) (Http2Session Client)

