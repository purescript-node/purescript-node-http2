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
  , altsvc
  , origin
  , onAltsvc
  , onOrigin
  , RequestOptions
  , request
  , request'
  ) where

import Prelude

import Data.Either (Either(..))
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

onClose :: forall peer. Http2Session peer -> Effect Unit -> Effect Unit
onClose session cb = runEffectFn2 onCloseImpl session cb

foreign import onCloseImpl :: forall peer. EffectFn2 (Http2Session peer) (Effect Unit) Unit

onConnect :: forall peer. Http2Session peer -> (Http2Session peer -> Socket -> Effect Unit) -> Effect Unit
onConnect h2s cb = runEffectFn2 onConnectImpl h2s $ mkEffectFn2 cb

foreign import onConnectImpl :: forall peer. EffectFn2 (Http2Session peer) (EffectFn2 (Http2Session peer) Socket Unit) Unit

onError :: forall peer. Http2Session peer -> (Error -> Effect Unit) -> Effect Unit
onError session cb = runEffectFn2 onErrorImpl session $ mkEffectFn1 cb

foreign import onErrorImpl :: forall peer. EffectFn2 (Http2Session peer) (EffectFn1 Error Unit) (Unit)

onFrameError :: forall peer. Http2Session peer -> (Int -> Int -> Int -> Effect Unit) -> Effect Unit
onFrameError session cb = runEffectFn2 onFrameErrorImpl session $ mkEffectFn3 cb

foreign import onFrameErrorImpl :: forall peer. EffectFn2 (Http2Session peer) (EffectFn3 Int Int Int Unit) (Unit)

onGoAway :: forall peer. Http2Session peer -> (Int -> Int -> Maybe ImmutableBuffer -> Effect Unit) -> Effect Unit
onGoAway session cb = runEffectFn2 onGoAwayImpl session $ mkEffectFn3 \c lsi buf ->
  cb c lsi (toMaybe buf)

foreign import onGoAwayImpl :: forall peer. EffectFn2 (Http2Session peer) (EffectFn3 Int Int (Nullable ImmutableBuffer) Unit) (Unit)

onLocalSettings :: forall peer. Http2Session peer -> (Settings -> Effect Unit) -> Effect Unit
onLocalSettings session cb = runEffectFn2 onLocalSettingsImpl session $ mkEffectFn1 cb

foreign import onLocalSettingsImpl :: forall peer. EffectFn2 (Http2Session peer) (EffectFn1 Settings Unit) (Unit)

onPing :: forall peer. Http2Session peer -> (Maybe ImmutableBuffer -> Effect Unit) -> Effect Unit
onPing sesson cb = runEffectFn2 onPingImpl sesson $ mkEffectFn1 \a -> cb $ toMaybe a

foreign import onPingImpl :: forall peer. EffectFn2 (Http2Session peer) (EffectFn1 (Nullable ImmutableBuffer) Unit) (Unit)

onRemoteSettings :: forall peer. Http2Session peer -> (Settings -> Effect Unit) -> Effect Unit
onRemoteSettings session cb = runEffectFn2 onRemoteSettingsImpl session $ mkEffectFn1 cb

foreign import onRemoteSettingsImpl :: forall peer. EffectFn2 (Http2Session peer) (EffectFn1 Settings Unit) (Unit)

onStream :: forall peer. Http2Session peer -> (Http2Stream peer -> Headers -> Number -> (Array String) -> Effect Unit) -> Effect Unit
onStream session cb = runEffectFn2 onStreamImpl session $ mkEffectFn4 cb

foreign import onStreamImpl :: forall peer. EffectFn2 (Http2Session peer) (EffectFn4 (Http2Stream peer) Headers Number (Array String) Unit) (Unit)

onTimeout :: forall peer. Http2Session peer -> Effect Unit -> Effect Unit
onTimeout session cb = runEffectFn2 onTimeoutImpl session cb

foreign import onTimeoutImpl :: forall peer. EffectFn2 (Http2Session peer) (Effect Unit) (Unit)

alpnProtocol :: forall peer. Http2Session peer -> Effect (Maybe String)
alpnProtocol session = map toMaybe $ runEffectFn1 alpnProtocolImpl session

foreign import alpnProtocolImpl :: forall peer. EffectFn1 (Http2Session peer) (Nullable String)

close :: forall peer. Http2Session peer -> Effect Unit
close session = runEffectFn1 closeImpl session

foreign import closeImpl :: forall peer. EffectFn1 (Http2Session peer) (Unit)

closed :: forall peer. Http2Session peer -> Effect Boolean
closed session = runEffectFn1 closedImpl session

foreign import closedImpl :: forall peer. EffectFn1 (Http2Session peer) (Boolean)

connecting :: forall peer. Http2Session peer -> Effect Boolean
connecting session = runEffectFn1 connectingImpl session

foreign import connectingImpl :: forall peer. EffectFn1 (Http2Session peer) (Boolean)

destroy :: forall peer. Http2Session peer -> Effect Unit
destroy session = runEffectFn1 destroyImpl session

foreign import destroyImpl :: forall peer. EffectFn1 (Http2Session peer) (Unit)

destroyWithError :: forall peer. Http2Session peer -> Error -> Effect Unit
destroyWithError s e = runEffectFn2 destroyWithErrorImpl s e

foreign import destroyWithErrorImpl :: forall peer. EffectFn2 (Http2Session peer) (Error) (Unit)

destroyWithCode :: forall peer. Http2Session peer -> Int -> Effect Unit
destroyWithCode s c = runEffectFn2 destroyWithCodeImpl s c

foreign import destroyWithCodeImpl :: forall peer. EffectFn2 (Http2Session peer) (Int) (Unit)

destroyWithErrorCode :: forall peer. Http2Session peer -> Error -> Int -> Effect Unit
destroyWithErrorCode s e c = runEffectFn3 destroyWithErrorCodeImpl s e c

foreign import destroyWithErrorCodeImpl :: forall peer. EffectFn3 (Http2Session peer) (Error) (Int) (Unit)

destroyed :: forall peer. Http2Session peer -> Effect Boolean
destroyed s = runEffectFn1 destroyedImpl s

foreign import destroyedImpl :: forall peer. EffectFn1 (Http2Session peer) (Boolean)

encrypted :: forall peer. Http2Session peer -> Effect (Maybe Boolean)
encrypted s = map toMaybe $ runEffectFn1 encryptedImpl s

foreign import encryptedImpl :: forall peer. EffectFn1 (Http2Session peer) (Nullable Boolean)

goAway :: forall peer. Http2Session peer -> Effect Unit
goAway s = runEffectFn1 goAwayImpl s

foreign import goAwayImpl :: forall peer. EffectFn1 (Http2Session peer) (Unit)

goAwayCode :: forall peer. Http2Session peer -> Int -> Effect Unit
goAwayCode s c = runEffectFn2 goAwayCodeImpl s c

foreign import goAwayCodeImpl :: forall peer. EffectFn2 (Http2Session peer) (Int) (Unit)

goAwayCodeLastStreamId :: forall peer. Http2Session peer -> Int -> Int -> Effect Unit
goAwayCodeLastStreamId s c lsi = runEffectFn3 goAwayCodeLastStreamIdImpl s c lsi

foreign import goAwayCodeLastStreamIdImpl :: forall peer. EffectFn3 (Http2Session peer) (Int) (Int) (Unit)

goAwayCodeLastStreamIdData :: forall peer. Http2Session peer -> Int -> Int -> ImmutableBuffer -> Effect Unit
goAwayCodeLastStreamIdData s c lsi buf = runEffectFn4 goAwayCodeLastStreamIdOpaqueDataImpl s c lsi buf

foreign import goAwayCodeLastStreamIdOpaqueDataImpl :: forall peer. EffectFn4 (Http2Session peer) (Int) (Int) (ImmutableBuffer) (Unit)

localSettings :: forall peer. Http2Session peer -> Effect Settings
localSettings s = runEffectFn1 localSettingsImpl s

foreign import localSettingsImpl :: forall peer. EffectFn1 (Http2Session peer) (Settings)

originSet :: forall peer. Http2Session peer -> Array String
originSet s = fromMaybe [] $ toMaybe $ runFn1 originSetImpl s

foreign import originSetImpl :: forall peer. Fn1 (Http2Session peer) (Nullable (Array String))

pendingSettingsAck :: forall peer. Http2Session peer -> Effect Boolean
pendingSettingsAck s = runEffectFn1 pendingSettingsAckImpl s

foreign import pendingSettingsAckImpl :: forall peer. EffectFn1 (Http2Session peer) (Boolean)

ping :: forall peer. Http2Session peer -> (Maybe Error -> Milliseconds -> ImmutableBuffer -> Effect Unit) -> Effect Boolean
ping s cb = runEffectFn2 pingImpl s $ mkEffectFn3 \err dur payload ->
  cb (toMaybe err) dur payload

foreign import pingImpl :: forall peer. EffectFn2 (Http2Session peer) (EffectFn3 (Nullable Error) Milliseconds ImmutableBuffer Unit) (Boolean)

pingPayload :: forall peer. Http2Session peer -> ImmutableBuffer -> (Maybe Error -> Milliseconds -> ImmutableBuffer -> Effect Unit) -> Effect Boolean
pingPayload s buf cb = runEffectFn3 pingPayloadImpl s buf $ mkEffectFn3 \err dur payload ->
  cb (toMaybe err) dur payload

foreign import pingPayloadImpl :: forall peer. EffectFn3 (Http2Session peer) ImmutableBuffer (EffectFn3 (Nullable Error) Milliseconds ImmutableBuffer Unit) (Boolean)

ref :: forall peer. Http2Session peer -> Effect Socket
ref s = runEffectFn1 refImpl s

foreign import refImpl :: forall peer. EffectFn1 (Http2Session peer) (Socket)

remoteSettings :: forall peer. Http2Session peer -> Effect Settings
remoteSettings s = runEffectFn1 remoteSettingsImpl s

foreign import remoteSettingsImpl :: forall peer. EffectFn1 (Http2Session peer) (Settings)

setLocalWindowSize :: forall peer. Http2Session peer -> Int -> Effect Unit
setLocalWindowSize s windowSize = runEffectFn2 setLocalWindowSizeImpl s windowSize

foreign import setLocalWindowSizeImpl :: forall peer. EffectFn2 (Http2Session peer) (Int) (Unit)

setTimeout :: forall peer. Http2Session peer -> Milliseconds -> Effect Unit -> Effect Unit
setTimeout s msecs cb = runEffectFn3 setTimeoutImpl s msecs cb

foreign import setTimeoutImpl :: forall peer. EffectFn3 (Http2Session peer) (Milliseconds) (Effect Unit) (Unit)

settings :: forall peer. Http2Session peer -> Settings -> (Maybe Error -> Settings -> Int -> Effect Unit) -> Effect Unit
settings s set cb = runEffectFn3 settingsImpl s set $ mkEffectFn3 \err set' duration ->
  cb (toMaybe err) set' duration

foreign import settingsImpl :: forall peer. EffectFn3 (Http2Session peer) (Settings) (EffectFn3 (Nullable Error) Settings Int Unit) (Unit)

socket :: forall peer. Http2Session peer -> Effect Socket
socket s = runEffectFn1 socketImpl s

foreign import socketImpl :: forall peer. EffectFn1 (Http2Session peer) (Socket)

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

state :: forall peer. Http2Session peer -> Effect Http2SessionState
state s = runEffectFn1 stateImpl s

foreign import stateImpl :: forall peer. EffectFn1 (Http2Session peer) (Http2SessionState)

type_ :: forall peer. Http2Session peer -> Int
type_ = runFn1 typeImpl

foreign import typeImpl :: forall peer. Fn1 (Http2Session peer) Int

unref :: forall peer. Http2Session peer -> Effect Socket
unref s = runEffectFn1 unrefImpl s

foreign import unrefImpl :: forall peer. EffectFn1 (Http2Session peer) (Socket)

altsvc :: Http2Session Server -> String -> Either Int String -> Effect Unit
altsvc s alt originOrStream = case originOrStream of
  Left streamId -> runEffectFn3 altsvcStreamImpl s alt streamId
  Right origin' -> runEffectFn3 altsvcOriginImpl s alt origin'

foreign import altsvcStreamImpl :: EffectFn3 (Http2Session Server) (String) Int (Unit)
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
