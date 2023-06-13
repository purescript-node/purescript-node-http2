-- | Note: an unencrypted HTTP/2 Server (i.e. `Http2Server` in Node.js)
-- | is simply not supported in this library.
module Node.Http2.Types where

-- | Type-level tag that indicates whether the stream/session/etc.
-- | is a server or client one.
data PeerType

foreign import data Client :: PeerType
foreign import data Server :: PeerType

foreign import data Http2Session :: PeerType -> Type

foreign import data Http2Stream :: PeerType -> Type

foreign import data Headers :: Type

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

foreign import data Http2SecureServer :: Type

foreign import data Http2ServerRequest :: Type
foreign import data Http2ServerResponse :: Type
