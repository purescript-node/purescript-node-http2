module Node.Http2.Settings
  ( defaultSettings
  , getDefaultSettings
  , getPackedSettings
  , getUnpackedSettings
  ) where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Http2.Types (Settings)

defaultSettings :: Settings
defaultSettings =
  { headerTableSize: 4096.0
  , enablePush: true
  , initialWindowSize: 65535.0
  , maxFrameSize: 16384.0
  , maxConcurrentStreams: 4_294_967_295.0
  , maxHeaderListSize: 65535.0
  , enableConnectProtocol: false
  }

foreign import getDefaultSettings :: Effect (Settings)

getPackedSettings :: Settings -> Effect ImmutableBuffer
getPackedSettings s = runEffectFn1 getPackedSettingsImpl s

foreign import getPackedSettingsImpl :: EffectFn1 (Settings) (ImmutableBuffer)

getUnpackedSettings :: ImmutableBuffer -> Effect Settings
getUnpackedSettings buf = runEffectFn1 getUnpackedSettingsImpl buf

foreign import getUnpackedSettingsImpl :: EffectFn1 (ImmutableBuffer) (Settings)
