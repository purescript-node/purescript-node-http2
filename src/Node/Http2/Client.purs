module Node.Http2.Client
  ( ConnectOptions
  , connect
  , connect'
  ) where

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Http2.Types (Client, Http2Session, Settings, TlsSecureContextOptions)
import Node.Net.Socket (Socket)
import Type.Row (type (+))

connect :: String -> Effect (Http2Session Client)
connect authority = runEffectFn1 connectAuthImpl authority

foreign import connectAuthImpl :: EffectFn1 (String) (Http2Session Client)

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
-- | `createConnection` <Function> An optional callback that receives the URL instance passed to connect and the options object, and returns any Duplex stream that is to be used as the connection for this session.
-- | `unknownProtocolTimeout` <number> Specifies a timeout in milliseconds that a server should wait when an 'unknownProtocol' event is emitted. If the socket has not been destroyed by that time the server will destroy it. Default: 10000.
-- |
-- | Note: `createConnection` is not supported for now.
type ConnectOptions :: (Type -> Type) -> Row Type -> Row Type
type ConnectOptions f r =
  ( maxDeflateDynamicTableSize :: f Int
  , maxSettings :: f Int
  , maxSessionMemory :: f Int
  , maxHeaderListPairs :: f Int
  , maxOutstandingPings :: f Int
  , maxReservedRemoteStreams :: f Int
  , maxSendHeaderBlockLength :: f Int
  , paddingStrategy :: f Int
  , peerMaxConcurrentStreams :: f Int
  , protocol :: f String
  , settings :: f Settings
  -- , createConnection :: Function -- not supported for now.
  , unknownProtocolTimeout :: f Int
  | r
  )

type TlsConnectOptions :: (Type -> Type) -> Row Type -> Row Type
type TlsConnectOptions f r =
  ( enableTrace :: f Boolean
  , socket :: f Socket
  , allowHalfOpen :: f Boolean
  , rejectUnauthorized :: f Boolean
  -- , pskCallback :: <Function
  , "ALPNProtocols" :: f (Array ImmutableBuffer)
  , servername :: f String
  -- , checkServerIdentity :: EffectFn2 String ? (Nullable Error) -- ignoring for now
  , session :: f ImmutableBuffer
  , minDHSize :: f Int
  , highWaterMark :: f Int
  -- , secureContext :: f TlsSecureContext -- ignoring for now
  -- , onread :: <Object> -- ignoring
  | r
  )

type TcpConnectOptions :: (Type -> Type) -> Row Type -> Row Type
type TcpConnectOptions f r =
  ( port :: f Int
  , host :: f String
  , localAddress :: f String
  , localPort :: f Int
  , family :: f Int
  -- , hints :: f number
  -- , lookup :: f Function
  , noDelay :: f Boolean
  , keepAlive :: f Boolean
  , keepAliveInitialDelay :: f Number -- is this milliseconds or seconds?
  -- , autoSelectFamily :: f Boolean
  -- , autoSelectFamilyAttemptTimeout :: f number
  | r
  )

connect'
  :: String
  -> ({ | ConnectOptions Maybe + TlsConnectOptions Maybe + TlsSecureContextOptions Maybe + TcpConnectOptions Maybe + () } -> { | ConnectOptions Maybe + TlsConnectOptions Maybe + TlsSecureContextOptions Maybe + TcpConnectOptions Maybe + () })
  -> Effect (Http2Session Client)
connect' authority buildOptions = do
  let
    o = buildOptions
      { maxDeflateDynamicTableSize: Nothing
      , maxSettings: Nothing
      , maxSessionMemory: Nothing
      , maxHeaderListPairs: Nothing
      , maxOutstandingPings: Nothing
      , maxReservedRemoteStreams: Nothing
      , maxSendHeaderBlockLength: Nothing
      , paddingStrategy: Nothing
      , peerMaxConcurrentStreams: Nothing
      , protocol: Nothing
      , settings: Nothing
      , unknownProtocolTimeout: Nothing
      -- TlsConnect
      , enableTrace: Nothing
      , socket: Nothing
      , allowHalfOpen: Nothing
      , rejectUnauthorized: Nothing
      , "ALPNProtocols": Nothing
      , servername: Nothing
      , session: Nothing
      , minDHSize: Nothing
      , highWaterMark: Nothing
      -- TlsSecureContext
      , ca: Nothing
      , cert: Nothing
      , sigalgs: Nothing
      , ciphers: Nothing
      , clientCertEngine: Nothing
      , crl: Nothing
      , dhparam: Nothing
      , ecdhCurve: Nothing
      , honorCipherOrder: Nothing
      , key: Nothing
      , privateKeyEngine: Nothing
      , privateKeyIdentifier: Nothing
      , maxVersion: Nothing
      , minVersion: Nothing
      , passphrase: Nothing
      , pfx: Nothing
      , secureOptions: Nothing
      , secureProtocol: Nothing
      , sessionIdContext: Nothing
      , ticketKeys: Nothing
      , sessionTimeout: Nothing
      -- TcpConnect
      , port: Nothing
      , host: Nothing
      , localAddress: Nothing
      , localPort: Nothing
      , family: Nothing
      , noDelay: Nothing
      , keepAlive: Nothing
      , keepAliveInitialDelay: Nothing
      }

    finalOptions :: { | ConnectOptions Unlift + TlsConnectOptions Unlift + TlsSecureContextOptions Unlift + TcpConnectOptions Unlift + () }
    finalOptions =
      { maxDeflateDynamicTableSize: fromMaybe undefined o.maxDeflateDynamicTableSize
      , maxSettings: fromMaybe undefined o.maxSettings
      , maxSessionMemory: fromMaybe undefined o.maxSessionMemory
      , maxHeaderListPairs: fromMaybe undefined o.maxHeaderListPairs
      , maxOutstandingPings: fromMaybe undefined o.maxOutstandingPings
      , maxReservedRemoteStreams: fromMaybe undefined o.maxReservedRemoteStreams
      , maxSendHeaderBlockLength: fromMaybe undefined o.maxSendHeaderBlockLength
      , paddingStrategy: fromMaybe undefined o.paddingStrategy
      , peerMaxConcurrentStreams: fromMaybe undefined o.peerMaxConcurrentStreams
      , protocol: fromMaybe undefined o.protocol
      , settings: fromMaybe undefined o.settings
      , unknownProtocolTimeout: fromMaybe undefined o.unknownProtocolTimeout
      -- TlsConnect
      , enableTrace: fromMaybe undefined o.enableTrace
      , socket: fromMaybe undefined o.socket
      , allowHalfOpen: fromMaybe undefined o.allowHalfOpen
      , rejectUnauthorized: fromMaybe undefined o.rejectUnauthorized
      , "ALPNProtocols": fromMaybe undefined o."ALPNProtocols"
      , servername: fromMaybe undefined o.servername
      , session: fromMaybe undefined o.session
      , minDHSize: fromMaybe undefined o.minDHSize
      , highWaterMark: fromMaybe undefined o.highWaterMark
      -- TlsSecureContext
      , ca: fromMaybe undefined o.ca
      , cert: fromMaybe undefined o.cert
      , sigalgs: fromMaybe undefined o.sigalgs
      , ciphers: fromMaybe undefined o.ciphers
      , clientCertEngine: fromMaybe undefined o.clientCertEngine
      , crl: fromMaybe undefined o.crl
      , dhparam: fromMaybe undefined o.dhparam
      , ecdhCurve: fromMaybe undefined o.ecdhCurve
      , honorCipherOrder: fromMaybe undefined o.honorCipherOrder
      , key: fromMaybe undefined o.key
      , privateKeyEngine: fromMaybe undefined o.privateKeyEngine
      , privateKeyIdentifier: fromMaybe undefined o.privateKeyIdentifier
      , maxVersion: fromMaybe undefined o.maxVersion
      , minVersion: fromMaybe undefined o.minVersion
      , passphrase: fromMaybe undefined o.passphrase
      , pfx: fromMaybe undefined o.pfx
      , secureOptions: fromMaybe undefined o.secureOptions
      , secureProtocol: fromMaybe undefined o.secureProtocol
      , sessionIdContext: fromMaybe undefined o.sessionIdContext
      , ticketKeys: fromMaybe undefined o.ticketKeys
      , sessionTimeout: fromMaybe undefined o.sessionTimeout
      -- TcpConnect
      , port: fromMaybe undefined o.port
      , host: fromMaybe undefined o.host
      , localAddress: fromMaybe undefined o.localAddress
      , localPort: fromMaybe undefined o.localPort
      , family: fromMaybe undefined o.family
      , noDelay: fromMaybe undefined o.noDelay
      , keepAlive: fromMaybe undefined o.keepAlive
      , keepAliveInitialDelay: fromMaybe undefined o.keepAliveInitialDelay
      }

  runEffectFn2 connectAuthOptionsImpl authority finalOptions

foreign import connectAuthOptionsImpl :: EffectFn2 (String) ({ | ConnectOptions Unlift + TlsConnectOptions Unlift + TlsSecureContextOptions Unlift + TcpConnectOptions Unlift + () }) (Http2Session Client)

type Unlift :: Type -> Type
type Unlift a = a

foreign import undefined :: forall a. a
