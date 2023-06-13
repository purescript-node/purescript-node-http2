module Node.Http2.Session
  ( onClose
  , onConnect
  , onError
  , onFrameError
  , onGoAway
  , onLocalSettings
  , onPing
  , onRemoteSettings
  , onStream
  , onTimeout
  , alpnProtocol
  , close
  , closed
  , connecting
  , destroy
  , destroyWithError
  , destroyWithCode
  , destroyWithErrorCode
  , destroyed
  , encrypted
  , goAway
  , goAwayCode
  , goAwayCodeLastStreamId
  , goAwayCodeLastStreamIdData
  , localSettings
  , originSet
  , pendingSettingsAck
  , ping
  , pingPayload
  , ref
  , remoteSettings
  , setLocalWindowSize
  , setTimeout
  , settings
  , socket
  , Http2SessionState
  , state
  , type_
  , unref
  , altsvcStreamId
  , altsvcOrigin
  , origin
  , onAltsvc
  , onOrigin
  , RequestOptions
  , request
  , request'
  ) where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, mkEffectFn1, mkEffectFn2, mkEffectFn3, mkEffectFn4, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Http2.Types (Client, Headers, Http2Session, Http2Stream, Server, Settings)
import Node.Net.Socket (Socket)

onClose :: forall endpoint. Http2Session endpoint -> Effect Unit -> Effect Unit
onClose session cb = runEffectFn2 onCloseImpl session cb

foreign import onCloseImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (Effect Unit) Unit

onConnect :: forall endpoint. Http2Session endpoint -> (Http2Session endpoint -> Socket -> Effect Unit) -> Effect Unit
onConnect h2s cb = runEffectFn2 onConnectImpl h2s $ mkEffectFn2 cb

foreign import onConnectImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (EffectFn2 (Http2Session endpoint) Socket Unit) Unit

onError :: forall endpoint. Http2Session endpoint -> (Error -> Effect Unit) -> Effect Unit
onError session cb = runEffectFn2 onErrorImpl session $ mkEffectFn1 cb

foreign import onErrorImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (EffectFn1 Error Unit) (Unit)

onFrameError :: forall endpoint. Http2Session endpoint -> (Int -> Int -> Int -> Effect Unit) -> Effect Unit
onFrameError session cb = runEffectFn2 onFrameErrorImpl session $ mkEffectFn3 cb

foreign import onFrameErrorImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (EffectFn3 Int Int Int Unit) (Unit)

onGoAway :: forall endpoint. Http2Session endpoint -> (Int -> Int -> Maybe ImmutableBuffer -> Effect Unit) -> Effect Unit
onGoAway session cb = runEffectFn2 onGoAwayImpl session $ mkEffectFn3 \c lsi buf ->
  cb c lsi (toMaybe buf)

foreign import onGoAwayImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (EffectFn3 Int Int (Nullable ImmutableBuffer) Unit) (Unit)

onLocalSettings :: forall endpoint. Http2Session endpoint -> (Settings -> Effect Unit) -> Effect Unit
onLocalSettings session cb = runEffectFn2 onLocalSettingsImpl session $ mkEffectFn1 cb

foreign import onLocalSettingsImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (EffectFn1 Settings Unit) (Unit)

onPing :: forall endpoint. Http2Session endpoint -> (Maybe ImmutableBuffer -> Effect Unit) -> Effect Unit
onPing sesson cb = runEffectFn2 onPingImpl sesson $ mkEffectFn1 \a -> cb $ toMaybe a

foreign import onPingImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (EffectFn1 (Nullable ImmutableBuffer) Unit) (Unit)

onRemoteSettings :: forall endpoint. Http2Session endpoint -> (Settings -> Effect Unit) -> Effect Unit
onRemoteSettings session cb = runEffectFn2 onRemoteSettingsImpl session $ mkEffectFn1 cb

foreign import onRemoteSettingsImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (EffectFn1 Settings Unit) (Unit)

onStream :: forall endpoint. Http2Session endpoint -> (Http2Stream endpoint -> Headers -> Number -> (Array String) -> Effect Unit) -> Effect Unit
onStream session cb = runEffectFn2 onStreamImpl session $ mkEffectFn4 cb

foreign import onStreamImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (EffectFn4 (Http2Stream endpoint) Headers Number (Array String) Unit) (Unit)

onTimeout :: forall endpoint. Http2Session endpoint -> Effect Unit -> Effect Unit
onTimeout session cb = runEffectFn2 onTimeoutImpl session cb

foreign import onTimeoutImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (Effect Unit) (Unit)

alpnProtocol :: forall endpoint. Http2Session endpoint -> Effect (Maybe String)
alpnProtocol session = map toMaybe $ runEffectFn1 alpnProtocolImpl session

foreign import alpnProtocolImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Nullable String)

close :: forall endpoint. Http2Session endpoint -> Effect Unit
close session = runEffectFn1 closeImpl session

foreign import closeImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Unit)

closed :: forall endpoint. Http2Session endpoint -> Effect Boolean
closed session = runEffectFn1 closedImpl session

foreign import closedImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Boolean)

connecting :: forall endpoint. Http2Session endpoint -> Effect Boolean
connecting session = runEffectFn1 connectingImpl session

foreign import connectingImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Boolean)

destroy :: forall endpoint. Http2Session endpoint -> Effect Unit
destroy session = runEffectFn1 destroyImpl session

foreign import destroyImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Unit)

destroyWithError :: forall endpoint. Http2Session endpoint -> Error -> Effect Unit
destroyWithError s e = runEffectFn2 destroyWithErrorImpl s e

foreign import destroyWithErrorImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (Error) (Unit)

destroyWithCode :: forall endpoint. Http2Session endpoint -> Int -> Effect Unit
destroyWithCode s c = runEffectFn2 destroyWithCodeImpl s c

foreign import destroyWithCodeImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (Int) (Unit)

destroyWithErrorCode :: forall endpoint. Http2Session endpoint -> Error -> Int -> Effect Unit
destroyWithErrorCode s e c = runEffectFn3 destroyWithErrorCodeImpl s e c

foreign import destroyWithErrorCodeImpl :: forall endpoint. EffectFn3 (Http2Session endpoint) (Error) (Int) (Unit)

destroyed :: forall endpoint. Http2Session endpoint -> Effect Boolean
destroyed s = runEffectFn1 destroyedImpl s

foreign import destroyedImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Boolean)

encrypted :: forall endpoint. Http2Session endpoint -> Effect (Maybe Boolean)
encrypted s = map toMaybe $ runEffectFn1 encryptedImpl s

foreign import encryptedImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Nullable Boolean)

goAway :: forall endpoint. Http2Session endpoint -> Effect Unit
goAway s = runEffectFn1 goAwayImpl s

foreign import goAwayImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Unit)

goAwayCode :: forall endpoint. Http2Session endpoint -> Int -> Effect Unit
goAwayCode s c = runEffectFn2 goAwayCodeImpl s c

foreign import goAwayCodeImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (Int) (Unit)

goAwayCodeLastStreamId :: forall endpoint. Http2Session endpoint -> Int -> Int -> Effect Unit
goAwayCodeLastStreamId s c lsi = runEffectFn3 goAwayCodeLastStreamIdImpl s c lsi

foreign import goAwayCodeLastStreamIdImpl :: forall endpoint. EffectFn3 (Http2Session endpoint) (Int) (Int) (Unit)

goAwayCodeLastStreamIdData :: forall endpoint. Http2Session endpoint -> Int -> Int -> ImmutableBuffer -> Effect Unit
goAwayCodeLastStreamIdData s c lsi buf = runEffectFn4 goAwayCodeLastStreamIdOpaqueDataImpl s c lsi buf

foreign import goAwayCodeLastStreamIdOpaqueDataImpl :: forall endpoint. EffectFn4 (Http2Session endpoint) (Int) (Int) (ImmutableBuffer) (Unit)

localSettings :: forall endpoint. Http2Session endpoint -> Effect Settings
localSettings s = runEffectFn1 localSettingsImpl s

foreign import localSettingsImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Settings)

originSet :: forall endpoint. Http2Session endpoint -> Array String
originSet s = fromMaybe [] $ toMaybe $ runFn1 originSetImpl s

foreign import originSetImpl :: forall endpoint. Fn1 (Http2Session endpoint) (Nullable (Array String))

pendingSettingsAck :: forall endpoint. Http2Session endpoint -> Effect Boolean
pendingSettingsAck s = runEffectFn1 pendingSettingsAckImpl s

foreign import pendingSettingsAckImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Boolean)

ping :: forall endpoint. Http2Session endpoint -> (Maybe Error -> Milliseconds -> ImmutableBuffer -> Effect Unit) -> Effect Boolean
ping s cb = runEffectFn2 pingImpl s $ mkEffectFn3 \err dur payload ->
  cb (toMaybe err) dur payload

foreign import pingImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (EffectFn3 (Nullable Error) Milliseconds ImmutableBuffer Unit) (Boolean)

pingPayload :: forall endpoint. Http2Session endpoint -> ImmutableBuffer -> (Maybe Error -> Milliseconds -> ImmutableBuffer -> Effect Unit) -> Effect Boolean
pingPayload s buf cb = runEffectFn3 pingPayloadImpl s buf $ mkEffectFn3 \err dur payload ->
  cb (toMaybe err) dur payload

foreign import pingPayloadImpl :: forall endpoint. EffectFn3 (Http2Session endpoint) ImmutableBuffer (EffectFn3 (Nullable Error) Milliseconds ImmutableBuffer Unit) (Boolean)

ref :: forall endpoint. Http2Session endpoint -> Effect Socket
ref s = runEffectFn1 refImpl s

foreign import refImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Socket)

remoteSettings :: forall endpoint. Http2Session endpoint -> Effect Settings
remoteSettings s = runEffectFn1 remoteSettingsImpl s

foreign import remoteSettingsImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Settings)

setLocalWindowSize :: forall endpoint. Http2Session endpoint -> Int -> Effect Unit
setLocalWindowSize s windowSize = runEffectFn2 setLocalWindowSizeImpl s windowSize

foreign import setLocalWindowSizeImpl :: forall endpoint. EffectFn2 (Http2Session endpoint) (Int) (Unit)

setTimeout :: forall endpoint. Http2Session endpoint -> Milliseconds -> Effect Unit -> Effect Unit
setTimeout s msecs cb = runEffectFn3 setTimeoutImpl s msecs cb

foreign import setTimeoutImpl :: forall endpoint. EffectFn3 (Http2Session endpoint) (Milliseconds) (Effect Unit) (Unit)

settings :: forall endpoint. Http2Session endpoint -> Settings -> (Maybe Error -> Settings -> Int -> Effect Unit) -> Effect Unit
settings s set cb = runEffectFn3 settingsImpl s set $ mkEffectFn3 \err set' duration ->
  cb (toMaybe err) set' duration

foreign import settingsImpl :: forall endpoint. EffectFn3 (Http2Session endpoint) (Settings) (EffectFn3 (Nullable Error) Settings Int Unit) (Unit)

socket :: forall endpoint. Http2Session endpoint -> Effect Socket
socket s = runEffectFn1 socketImpl s

foreign import socketImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Socket)

-- | `effectiveLocalWindowSize` <number> The current local (receive) flow control window size for the Http2Session peer.
-- | `effectiveRecvDataLength` <number> The current number of bytes that have been received since the last flow control WINDOW_UPDATE.
-- | `nextStreamID` <number> The numeric identifier to be used the next time a new Http2Stream is created by this Http2Session peer.
-- | `localWindowSize` <number> The number of bytes that the remote peer can send without receiving a WINDOW_UPDATE.
-- | `lastProcStreamID` <number> The numeric id of the Http2Stream for which a HEADERS or DATA frame was most recently received.
-- | `remoteWindowSize` <number> The number of bytes that this Http2Session peer may send without receiving a WINDOW_UPDATE.
-- | `outboundQueueSize` <number> The number of frames currently within the outbound queue for this Http2Session peer.
-- | `deflateDynamicTableSize` <number> The current size in bytes of the outbound header compression state table.
-- | `inflateDynamicTableSize` <number> The current size in bytes of the inbound header compression state table.
type Http2SessionState =
  { effectiveLocalWindowSize :: Int
  , effectiveRecvDataLength :: Int
  , nextStreamID :: Int
  , localWindowSize :: Int
  , lastProcStreamID :: Int
  , remoteWindowSize :: Int
  , outboundQueueSize :: Int
  , deflateDynamicTableSize :: Int
  , inflateDynamicTableSize :: Int
  }

state :: forall endpoint. Http2Session endpoint -> Effect Http2SessionState
state s = runEffectFn1 stateImpl s

foreign import stateImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Http2SessionState)

type_ :: forall endpoint. Http2Session endpoint -> Int
type_ = runFn1 typeImpl

foreign import typeImpl :: forall endpoint. Fn1 (Http2Session endpoint) Int

unref :: forall endpoint. Http2Session endpoint -> Effect Socket
unref s = runEffectFn1 unrefImpl s

foreign import unrefImpl :: forall endpoint. EffectFn1 (Http2Session endpoint) (Socket)

altsvcStreamId :: Http2Session Server -> String -> Int -> Effect Unit
altsvcStreamId s alt streamId = runEffectFn3 altsvcStreamImpl s alt streamId

foreign import altsvcStreamImpl :: EffectFn3 (Http2Session Server) (String) Int (Unit)

altsvcOrigin :: Http2Session Server -> String -> String -> Effect Unit
altsvcOrigin s alt origin' = runEffectFn3 altsvcOriginImpl s alt origin'

foreign import altsvcOriginImpl :: EffectFn3 (Http2Session Server) (String) String (Unit)

origin :: Http2Session Server -> Array String -> Effect Unit
origin s o = runEffectFn2 originImpl s o

foreign import originImpl :: EffectFn2 (Http2Session Server) (Array String) (Unit)

onAltsvc :: Http2Session Client -> (String -> String -> Int -> Effect Unit) -> Effect Unit
onAltsvc s cb = runEffectFn2 onAltsvcImpl s $ mkEffectFn3 cb

foreign import onAltsvcImpl :: EffectFn2 (Http2Session Client) (EffectFn3 String String Int Unit) (Unit)

onOrigin :: Http2Session Client -> (Array String -> Effect Unit) -> Effect Unit
onOrigin s cb = runEffectFn2 onOriginImpl s $ mkEffectFn1 cb

foreign import onOriginImpl :: EffectFn2 (Http2Session Client) (EffectFn1 (Array String) Unit) (Unit)

-- | `endStream` <boolean> true if the Http2Stream writable side should be closed initially, such as when sending a GET request that should not expect a payload body.
-- | `exclusive` <boolean> When true and parent identifies a parent Stream, the created stream is made the sole direct dependency of the parent, with all other existing dependents made a dependent of the newly created stream. Default: false.
-- | `parent` <number> Specifies the numeric identifier of a stream the newly created stream is dependent on.
-- | `weight` <number> Specifies the relative dependency of a stream in relation to other streams with the same parent. The value is a number between 1 and 256 (inclusive).
-- | `waitForTrailers` <boolean> When true, the Http2Stream will emit the 'wantTrailers' event after the final DATA frame has been sent.
-- |
-- | Note: `signal` <AbortSignal> option is intentionally not supported
type RequestOptions =
  { endStream :: Boolean
  , exclusive :: Boolean
  , parent :: Number
  , weight :: Number
  , waitForTrailers :: Boolean
  }

request :: Http2Session Client -> Headers -> Effect (Http2Stream Client)
request s h = runEffectFn2 requestHeadersImpl s h

foreign import requestHeadersImpl :: EffectFn2 (Http2Session Client) (Headers) (Http2Stream Client)

request' :: Http2Session Client -> Headers -> RequestOptions -> Effect (Http2Stream Client)
request' c h o = runEffectFn3 requestHeadersOptionsImpl c h o

foreign import requestHeadersOptionsImpl :: EffectFn3 (Http2Session Client) (Headers) (RequestOptions) (Http2Stream Client)
