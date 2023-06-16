-- | Note: an unencrypted HTTP/2 Server (i.e. `Http2Server` in Node.js)
-- | is simply not supported in this library.
module Node.Http2.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Node.Http2.PaddingStrategy (PaddingStrategy)
import Node.TLS.Types (Endpoint)

-- | `Http2Session` extends `EventEmitter`
foreign import data Http2Session :: Endpoint -> Type

-- | `Http2Session` extends `Duplex` and `EventEmitter`
foreign import data Http2Stream :: Endpoint -> Type

foreign import data Headers :: Type

newtype StreamId = StreamId Int

derive instance Eq StreamId
derive instance Ord StreamId
derive instance Newtype StreamId _
derive instance Generic StreamId _
derive newtype instance Show StreamId

-- | A StreamId of 0, indicating the ID does not refer to a stream
-- | but to the session's connection as a whole.
connectionId :: StreamId
connectionId = StreamId 0

-- | `headerTableSize` <number> Specifies the maximum number of bytes used for header compression. The minimum allowed value is 0. The maximum allowed value is 232-1. Default: 4096.
-- | `enablePush` <boolean> Specifies true if HTTP/2 Push Streams are to be permitted on the Http2Session instances. Default: true.
-- | `initialWindowSize` <number> Specifies the sender's initial window size in bytes for stream-level flow control. The minimum allowed value is 0. The maximum allowed value is 232-1. Default: 65535.
-- | `maxFrameSize` <number> Specifies the size in bytes of the largest frame payload. The minimum allowed value is 16,384. The maximum allowed value is 224-1. Default: 16384.
-- | `maxConcurrentStreams` <number> Specifies the maximum number of concurrent streams permitted on an Http2Session. There is no default value which implies, at least theoretically, 232^1 streams may be open concurrently at any given time in an Http2Session. The minimum value is 0. The maximum allowed value is 232^1. Default: 4294967295.
-- | `maxHeaderListSize` <number> Specifies the maximum size (uncompressed octets) of header list that will be accepted. The minimum allowed value is 0. The maximum allowed value is 232^1. Default: 65535.
-- | `enableConnectProtocol`<boolean> Specifies true if the "Extended Connect Protocol" defined by RFC 8441 is to be enabled. This setting is only meaningful if sent by the server. Once the enableConnectProtocol setting has been enabled for a given Http2Session, it cannot be disabled. Default: false.
-- |
-- | Note: the `maxHeaderSize` alias is intentionally not supported
type Settings =
  { headerTableSize :: Number
  , enablePush :: Boolean
  , initialWindowSize :: Number
  , maxFrameSize :: Number
  , maxConcurrentStreams :: Number
  , maxHeaderListSize :: Number
  , enableConnectProtocol :: Boolean
  }

-- | `Http2SecureServer` extends `TLS.Server`, `Net.Server` and `EventEmitter`
foreign import data Http2SecureServer :: Type

-- | Type is provided below, but the corresponding bindings for the Compatibility API
-- | is not provided.
foreign import data Http2ServerRequest :: Type

-- | Type is provided below, but the corresponding bindings for the Compatibility API
-- | is not provided.
foreign import data Http2ServerResponse :: Type

-- | `allowHTTP1` <boolean> Incoming client connections that do not support HTTP/2 will be downgraded to HTTP/1.x when set to true. See the 'unknownProtocol' event. See ALPN negotiation. Default: false.
-- | `maxDeflateDynamicTableSize` <number> Sets the maximum dynamic table size for deflating header fields. Default: 4Kib.
-- | `maxSettings` <number> Sets the maximum number of settings entries per SETTINGS frame. The minimum value allowed is 1. Default: 32.
-- | `maxSessionMemory`<number> Sets the maximum memory that the Http2Session is permitted to use. The value is expressed in terms of number of megabytes, e.g. 1 equal 1 megabyte. The minimum value allowed is 1. This is a credit based limit, existing Http2Streams may cause this limit to be exceeded, but new Http2Stream instances will be rejected while this limit is exceeded. The current number of Http2Stream sessions, the current memory use of the header compression tables, current data queued to be sent, and unacknowledged PING and SETTINGS frames are all counted towards the current limit. Default: 10.
-- | `maxHeaderListPairs` <number> Sets the maximum number of header entries. This is similar to server.maxHeadersCount or request.maxHeadersCount in the node:http module. The minimum value is 4. Default: 128.
-- | `maxOutstandingPings` <number> Sets the maximum number of outstanding, unacknowledged pings. Default: 10.
-- | `maxSendHeaderBlockLength` <number> Sets the maximum allowed size for a serialized, compressed block of headers. Attempts to send headers that exceed this limit will result in a 'frameError' event being emitted and the stream being closed and destroyed.
-- | `paddingStrategy` <number> Strategy used for determining the amount of padding to use for HEADERS and DATA frames. Default: http2.constants.PADDING_STRATEGY_NONE. Value may be one of:
-- |     - http2`.constants.PADDING_STRATEGY_NONE: No padding is applied.
-- |     - http2`.constants.PADDING_STRATEGY_MAX: The maximum amount of padding, determined by the internal implementation, is applied.
-- |     - http2`.constants.PADDING_STRATEGY_ALIGNED: Attempts to apply enough padding to ensure that the total frame length, including the 9-byte header, is a multiple of 8. For each frame, there is a maximum allowed number of padding bytes that is determined by current flow control state and settings. If this maximum is less than the calculated amount needed to ensure alignment, the maximum is used and the total frame length is not necessarily aligned at 8 bytes.
-- | `peerMaxConcurrentStreams` <number> Sets the maximum number of concurrent streams for the remote peer as if a SETTINGS frame had been received. Will be overridden if the remote peer sets its own value for maxConcurrentStreams. Default: 100.
-- | `maxSessionInvalidFrames` <integer> Sets the maximum number of invalid frames that will be tolerated before the session is closed. Default: 1000.
-- | `maxSessionRejectedStreams` <integer> Sets the maximum number of rejected upon creation streams that will be tolerated before the session is closed. Each rejection is associated with an NGHTTP2_ENHANCE_YOUR_CALM error that should tell the peer to not open any more streams, continuing to open streams is therefore regarded as a sign of a misbehaving peer. Default: 100.
-- | `settings` <HTTP/2 Settings Object> The initial settings to send to the remote peer upon connection.
-- | `...:` Any tls.createServer() options can be provided. For servers, the identity options (pfx or key/cert) are usually required.
-- | `origins` <string[]> An array of origin strings to send within an ORIGIN frame immediately following creation of a new server Http2Session.
-- | `unknownProtocolTimeout` <number> Specifies a timeout in milliseconds that a server should wait when an 'unknownProtocol' event is emitted. If the socket has not been destroyed by that time the server will destroy it. Default: 10000.
type Http2CreateSecureServerOptions :: Row Type -> Row Type
type Http2CreateSecureServerOptions r =
  ( allowHTTP1 :: Boolean
  , maxDeflateDynamicTableSize :: Int
  , maxSettings :: Int
  , maxSessionMemory :: Int
  , maxHeaderListPairs :: Int
  , maxOutstandingPings :: Int
  , maxSendHeaderBlockLength :: Int
  , paddingStrategy :: PaddingStrategy
  , peerMaxConcurrentStreams :: Int
  , maxSessionInvalidFrames :: Int
  , maxSessionRejectedStreams :: Int
  , settings :: Settings
  , origins :: Array String
  , unknownProtocolTimeout :: Int
  | r
  )

-- | `maxDeflateDynamicTableSize` <number> Sets the maximum dynamic table size for deflating header fields. Default: 4Kib.
-- | `maxSettings` <number> Sets the maximum number of settings entries per SETTINGS frame. The minimum value allowed is 1. Default: 32.
-- | `maxSessionMemory`<number> Sets the maximum memory that the Http2Session is permitted to use. The value is expressed in terms of number of megabytes, e.g. 1 equal 1 megabyte. The minimum value allowed is 1. This is a credit based limit, existing Http2Streams may cause this limit to be exceeded, but new Http2Stream instances will be rejected while this limit is exceeded. The current number of Http2Stream sessions, the current memory use of the header compression tables, current data queued to be sent, and unacknowledged PING and SETTINGS frames are all counted towards the current limit. Default: 10.
-- | `maxHeaderListPairs` <number> Sets the maximum number of header entries. This is similar to server.maxHeadersCount or request.maxHeadersCount in the node:http module. The minimum value is 1. Default: 128.
-- | `maxOutstandingPings` <number> Sets the maximum number of outstanding, unacknowledged pings. Default: 10.
-- | `maxReservedRemoteStreams` <number> Sets the maximum number of reserved push streams the client will accept at any given time. Once the current number of currently reserved push streams exceeds reaches this limit, new push streams sent by the server will be automatically rejected. The minimum allowed value is 0. The maximum allowed value is 232-1. A negative value sets this option to the maximum allowed value. Default: 200.
-- | `maxSendHeaderBlockLength` <number> Sets the maximum allowed size for a serialized, compressed block of headers. Attempts to send headers that exceed this limit will result in a 'frameError' event being emitted and the stream being closed and destroyed.
-- | `paddingStrategy` <number> Strategy used for determining the amount of padding to use for HEADERS and DATA frames. Default: http2.constants.PADDING_STRATEGY_NONE. Value may be one of:
-- | `peerMaxConcurrentStreams` <number> Sets the maximum number of concurrent streams for the remote peer as if a SETTINGS frame had been received. Will be overridden if the remote peer sets its own value for maxConcurrentStreams. Default: 100.
-- | `protocol` <string> The protocol to connect with, if not set in the authority. Value may be either 'http:' or 'https:'. Default: 'https:'
-- | `settings` <HTTP/2 Settings Object> The initial settings to send to the remote peer upon connection.
-- | `unknownProtocolTimeout` <number> Specifies a timeout in milliseconds that a server should wait when an 'unknownProtocol' event is emitted. If the socket has not been destroyed by that time the server will destroy it. Default: 10000.
-- |
-- | Note: `createConnection` is not supported for now.
type Http2ClientConnectOptions :: Row Type -> Row Type
type Http2ClientConnectOptions r =
  ( maxDeflateDynamicTableSize :: Int
  , maxSettings :: Int
  , maxSessionMemory :: Int
  , maxHeaderListPairs :: Int
  , maxOutstandingPings :: Int
  , maxReservedRemoteStreams :: Int
  , maxSendHeaderBlockLength :: Int
  , paddingStrategy :: Int
  , peerMaxConcurrentStreams :: Int
  , protocol :: String
  , settings :: Settings
  , unknownProtocolTimeout :: Int
  | r
  )
