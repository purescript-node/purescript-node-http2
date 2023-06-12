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

foreign import data Settings :: Type

foreign import data Http2SecureServer :: Type

foreign import data Http2ServerRequest :: Type
foreign import data Http2ServerResponse :: Type
