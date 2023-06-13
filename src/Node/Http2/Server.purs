module Node.Http2.Server
  ( Http2CreateSecureServerOptions
  , TlsCreateServerOptions
  , TlsSecureContextOptions
  , NetCreateServerOptions
  , createSecureServer
  , listen
  , onCheckContinue
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

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, mkEffectFn1, mkEffectFn2, mkEffectFn4, runEffectFn1, runEffectFn2)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Http2.Types (Headers, Http2SecureServer, Http2ServerRequest, Http2ServerResponse, Http2Session, Http2Stream, Server, Settings)
import Node.Stream (Duplex)
import Type.Row (type (+))

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
type Http2CreateSecureServerOptions :: (Type -> Type) -> Row Type -> Row Type
type Http2CreateSecureServerOptions f r =
  ( allowHTTP1 :: f Boolean
  , maxDeflateDynamicTableSize :: f Int
  , maxSettings :: f Int
  , maxSessionMemory :: f Int
  , maxHeaderListPairs :: f Int
  , maxOutstandingPings :: f Int
  , maxSendHeaderBlockLength :: f Int
  , paddingStrategy :: f Int
  , peerMaxConcurrentStreams :: f Int
  , maxSessionInvalidFrames :: f Int
  , maxSessionRejectedStreams :: f Int
  , settings :: f Settings
  , origins :: f (Array String)
  , unknownProtocolTimeout :: f Int
  | r
  )

-- | `ALPNProtocols`: <string[]> | <Buffer[]> | <TypedArray[]> | <DataView[]> | <Buffer> | <TypedArray> | <DataView> An array of strings, Buffers, TypedArrays, or DataViews, or a single Buffer, TypedArray, or DataView containing the supported ALPN protocols. Buffers should have the format [len][name][len][name]... e.g. 0x05hello0x05world, where the first byte is the length of the next protocol name. Passing an array is usually much simpler, e.g. ['hello', 'world']. (Protocols should be ordered by their priority.)
-- | `clientCertEngine` <string> Name of an OpenSSL engine which can provide the client certificate.
-- | `enableTrace` <boolean> If true, tls.TLSSocket.enableTrace() will be called on new connections. Tracing can be enabled after the secure connection is established, but this option must be used to trace the secure connection setup. Default: false.
-- | `handshakeTimeout` <number> Abort the connection if the SSL/TLS handshake does not finish in the specified number of milliseconds. A 'tlsClientError' is emitted on the tls.Server object whenever a handshake times out. Default: 120000 (120 seconds).
-- | `rejectUnauthorized` <boolean> If not false the server will reject any connection which is not authorized with the list of supplied CAs. This option only has an effect if requestCert is true. Default: true.
-- | `requestCert` <boolean> If true the server will request a certificate from clients that connect and attempt to verify that certificate. Default: false.
-- | `SNICallback`(servername, callback) <Function> A function that will be called if the client supports SNI TLS extension. Two arguments will be passed when called: servername and callback. callback is an error-first callback that takes two optional arguments: error and ctx. ctx, if provided, is a SecureContext instance. tls.createSecureContext() can be used to get a proper SecureContext. If callback is called with a falsy ctx argument, the default secure context of the server will be used. If SNICallback wasn't provided the default callback with high-level API will be used (see below).
-- | `pskCallback` <Function>
-- |    - socket: <tls.TLSSocket> the server tls.TLSSocket instance for this connection.
-- |    - identity: <string> identity parameter sent from the client.
-- |    - Returns: <Buffer> | <TypedArray> | <DataView> pre-shared key that must either be a buffer or null to stop the negotiation process. Returned PSK must be compatible with the selected cipher's digest.
-- |    When negotiating TLS-PSK (pre-shared keys), this function is called with the identity provided by the client. If the return value is null the negotiation process will stop and an "unknown_psk_identity" alert message will be sent to the other party. If the server wishes to hide the fact that the PSK identity was not known, the callback must provide some random data as psk to make the connection fail with "decrypt_error" before negotiation is finished. PSK ciphers are disabled by default, and using TLS-PSK thus requires explicitly specifying a cipher suite with the ciphers option. More information can be found in the RFC 4279.
-- | `pskIdentityHint` <string> optional hint to send to a client to help with selecting the identity during TLS-PSK negotiation. Will be ignored in TLS 1.3. Upon failing to set pskIdentityHint 'tlsClientError' will be emitted with 'ERR_TLS_PSK_SET_IDENTIY_HINT_FAILED' code.
type TlsCreateServerOptions :: (Type -> Type) -> Row Type -> Row Type
type TlsCreateServerOptions f r =
  ( "ALPNProtocols" :: f (Array String)
  , enableTrace :: f Boolean
  , handshakeTimeout :: f Int
  , rejectUnauthorized :: f Boolean
  , requestCert :: f Boolean
  -- , "SNICallback" :: -- Just ignoring this for now...
  -- , pskCallback :: -- And this one, too. f (TlsSocket -> String -> Effect ImmutableBuffer)
  , pskIdentityHint :: f String
  | r
  )

-- | `ca` <string> | <string[]> | <Buffer> | <Buffer[]> Optionally override the trusted CA certificates. Default is to trust the well-known CAs curated by Mozilla. Mozilla's CAs are completely replaced when CAs are explicitly specified using this option. The value can be a string or Buffer, or an Array of strings and/or Buffers. Any string or Buffer can contain multiple PEM CAs concatenated together. The peer's certificate must be chainable to a CA trusted by the server for the connection to be authenticated. When using certificates that are not chainable to a well-known CA, the certificate's CA must be explicitly specified as a trusted or the connection will fail to authenticate. If the peer uses a certificate that doesn't match or chain to one of the default CAs, use the ca option to provide a CA certificate that the peer's certificate can match or chain to. For self-signed certificates, the certificate is its own CA, and must be provided. For PEM encoded certificates, supported types are "TRUSTED CERTIFICATE", "X509 CERTIFICATE", and "CERTIFICATE". See also tls.rootCertificates.
-- | `cert` <string> | <string[]> | <Buffer> | <Buffer[]> Cert chains in PEM format. One cert chain should be provided per private key. Each cert chain should consist of the PEM formatted certificate for a provided private key, followed by the PEM formatted intermediate certificates (if any), in order, and not including the root CA (the root CA must be pre-known to the peer, see ca). When providing multiple cert chains, they do not have to be in the same order as their private keys in key. If the intermediate certificates are not provided, the peer will not be able to validate the certificate, and the handshake will fail.
-- | `sigalgs` <string> Colon-separated list of supported signature algorithms. The list can contain digest algorithms (SHA256, MD5 etc.), public key algorithms (RSA-PSS, ECDSA etc.), combination of both (e.g 'RSA+SHA384') or TLS v1.3 scheme names (e.g. rsa_pss_pss_sha512). See OpenSSL man pages for more info.
-- | `ciphers` <string> Cipher suite specification, replacing the default. For more information, see Modifying the default TLS cipher suite. Permitted ciphers can be obtained via tls.getCiphers(). Cipher names must be uppercased in order for OpenSSL to accept them.
-- | `clientCertEngine` <string> Name of an OpenSSL engine which can provide the client certificate.
-- | `crl` <string> | <string[]> | <Buffer> | <Buffer[]> PEM formatted CRLs (Certificate Revocation Lists).
-- | `dhparam` <string> | <Buffer> 'auto' or custom Diffie-Hellman parameters, required for non-ECDHE perfect forward secrecy. If omitted or invalid, the parameters are silently discarded and DHE ciphers will not be available. ECDHE-based perfect forward secrecy will still be available.
-- | `ecdhCurve` <string> A string describing a named curve or a colon separated list of curve NIDs or names, for example P-521:P-384:P-256, to use for ECDH key agreement. Set to auto to select the curve automatically. Use crypto.getCurves() to obtain a list of available curve names. On recent releases, openssl ecparam -list_curves will also display the name and description of each available elliptic curve. Default: tls.DEFAULT_ECDH_CURVE.
-- | `honorCipherOrder` <boolean> Attempt to use the server's cipher suite preferences instead of the client's. When true, causes SSL_OP_CIPHER_SERVER_PREFERENCE to be set in secureOptions, see OpenSSL Options for more information.
-- | `key` <string> | <string[]> | <Buffer> | <Buffer[]> | <Object[]> Private keys in PEM format. PEM allows the option of private keys being encrypted. Encrypted keys will be decrypted with options.passphrase. Multiple keys using different algorithms can be provided either as an array of unencrypted key strings or buffers, or an array of objects in the form {pem: <string|buffer>[, passphrase: <string>]}. The object form can only occur in an array. object.passphrase is optional. Encrypted keys will be decrypted with object.passphrase if provided, or options.passphrase if it is not.
-- | `privateKeyEngine` <string> Name of an OpenSSL engine to get private key from. Should be used together with privateKeyIdentifier.
-- | `privateKeyIdentifier` <string> Identifier of a private key managed by an OpenSSL engine. Should be used together with privateKeyEngine. Should not be set together with key, because both options define a private key in different ways.
-- | `maxVersion` <string> Optionally set the maximum TLS version to allow. One of 'TLSv1.3', 'TLSv1.2', 'TLSv1.1', or 'TLSv1'. Cannot be specified along with the secureProtocol option; use one or the other. Default: tls.DEFAULT_MAX_VERSION.
-- | `minVersion` <string> Optionally set the minimum TLS version to allow. One of 'TLSv1.3', 'TLSv1.2', 'TLSv1.1', or 'TLSv1'. Cannot be specified along with the secureProtocol option; use one or the other. Avoid setting to less than TLSv1.2, but it may be required for interoperability. Default: tls.DEFAULT_MIN_VERSION.
-- | `passphrase` <string> Shared passphrase used for a single private key and/or a PFX.
-- | `pfx` <string> | <string[]> | <Buffer> | <Buffer[]> | <Object[]> PFX or PKCS12 encoded private key and certificate chain. pfx is an alternative to providing key and cert individually. PFX is usually encrypted, if it is, passphrase will be used to decrypt it. Multiple PFX can be provided either as an array of unencrypted PFX buffers, or an array of objects in the form {buf: <string|buffer>[, passphrase: <string>]}. The object form can only occur in an array. object.passphrase is optional. Encrypted PFX will be decrypted with object.passphrase if provided, or options.passphrase if it is not.
-- | `secureOptions` <number> Optionally affect the OpenSSL protocol behavior, which is not usually necessary. This should be used carefully if at all! Value is a numeric bitmask of the SSL_OP_* options from OpenSSL Options.
-- | `secureProtocol` <string> Legacy mechanism to select the TLS protocol version to use, it does not support independent control of the minimum and maximum version, and does not support limiting the protocol to TLSv1.3. Use minVersion and maxVersion instead. The possible values are listed as SSL_METHODS, use the function names as strings. For example, use 'TLSv1_1_method' to force TLS version 1.1, or 'TLS_method' to allow any TLS protocol version up to TLSv1.3. It is not recommended to use TLS versions less than 1.2, but it may be required for interoperability. Default: none, see minVersion.
-- | `sessionIdContext` <string> Opaque identifier used by servers to ensure session state is not shared between applications. Unused by clients.
-- | `ticketKeys`: <Buffer> 48-bytes of cryptographically strong pseudorandom data. See Session Resumption for more information.
-- | `sessionTimeout` <number> The number of seconds after which a TLS session created by the server will no longer be resumable. See Session Resumption for more information. Default: 300.
type TlsSecureContextOptions :: (Type -> Type) -> Row Type -> Row Type
type TlsSecureContextOptions f r =
  ( ca :: f (Array ImmutableBuffer)
  , cert :: f (Array ImmutableBuffer)
  , sigalgs :: f String
  , ciphers :: f String
  , clientCertEngine :: f String
  , crl :: f (Array ImmutableBuffer)
  , dhparam :: f (Array ImmutableBuffer)
  , ecdhCurve :: f String
  , honorCipherOrder :: f Boolean
  , key :: f (Array ImmutableBuffer)
  , privateKeyEngine :: f String
  , privateKeyIdentifier :: f String
  , maxVersion :: f String
  , minVersion :: f String
  , passphrase :: f String
  , pfx :: f (Array ImmutableBuffer)
  , secureOptions :: f Int
  , secureProtocol :: f String
  , sessionIdContext :: f String
  , ticketKeys :: f ImmutableBuffer
  , sessionTimeout :: f Int
  | r
  )

-- | `allowHalfOpen` <boolean> If set to false, then the socket will automatically end the writable side when the readable side ends. Default: false.
-- | `pauseOnConnet` <boolean> Indicates whether the socket should be paused on incoming connections. Default: false.
-- | `noDelay` <booean> If set to true, it disables the use of Nagle's algorithm immediately after a new incoming connection is received. Default: false.
-- | `keepAlve` <boolean> If set to true, it enables keep-alive functionality on the socket immediately after a new incoming connection is received, similarly on what is done in socket.setKeepAlive([enable][, initialDelay]). Default: false.
-- | `keepAlivInitialDelay` <number> If set to a positive number, it sets the initial delay before the first keepalive probe is sent on an idle socket.Default: 0.
type NetCreateServerOptions :: (Type -> Type) -> Row Type -> Row Type
type NetCreateServerOptions f r =
  ( allowHalfOpen :: f Boolean
  , pauseOnConnect :: f Boolean
  , noDelay :: f Boolean
  , keepAlive :: f Boolean
  , keepAliveInitialDelay :: f Milliseconds
  | r
  )

-- | Yes, there really are that many possible options...
-- | Intended usage:
-- | ```
-- | createSecureServer (_ 
-- |   { settings = Just mySettings
-- |   })
-- | ```
createSecureServer
  :: ( { | Http2CreateSecureServerOptions Maybe + TlsCreateServerOptions Maybe + TlsSecureContextOptions Maybe + NetCreateServerOptions Maybe + () }
       -> { | Http2CreateSecureServerOptions Maybe + TlsCreateServerOptions Maybe + TlsSecureContextOptions Maybe + NetCreateServerOptions Maybe + () }
     )
  -> Effect Http2SecureServer
createSecureServer buildOptions = do
  let
    o = buildOptions
      { allowHTTP1: Nothing
      , maxDeflateDynamicTableSize: Nothing
      , maxSettings: Nothing
      , maxSessionMemory: Nothing
      , maxHeaderListPairs: Nothing
      , maxOutstandingPings: Nothing
      , maxSendHeaderBlockLength: Nothing
      , paddingStrategy: Nothing
      , peerMaxConcurrentStreams: Nothing
      , maxSessionInvalidFrames: Nothing
      , maxSessionRejectedStreams: Nothing
      , settings: Nothing
      , origins: Nothing
      , unknownProtocolTimeout: Nothing
      -- tls create server options
      , "ALPNProtocols": Nothing
      , enableTrace: Nothing
      , handshakeTimeout: Nothing
      , rejectUnauthorized: Nothing
      , requestCert: Nothing
      , pskIdentityHint: Nothing
      -- tls secure context options
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
      -- net create server options
      , allowHalfOpen: Nothing
      , pauseOnConnect: Nothing
      , noDelay: Nothing
      , keepAlive: Nothing
      , keepAliveInitialDelay: Nothing
      }

    options' :: { | Http2CreateSecureServerOptions Unlift + TlsCreateServerOptions Unlift + TlsSecureContextOptions Unlift + NetCreateServerOptions Unlift + () }
    options' =
      -- Http2
      { allowHTTP1: fromMaybe undefined o.allowHTTP1
      , maxDeflateDynamicTableSize: fromMaybe undefined o.maxDeflateDynamicTableSize
      , maxSettings: fromMaybe undefined o.maxSettings
      , maxSessionMemory: fromMaybe undefined o.maxSessionMemory
      , maxHeaderListPairs: fromMaybe undefined o.maxHeaderListPairs
      , maxOutstandingPings: fromMaybe undefined o.maxOutstandingPings
      , maxSendHeaderBlockLength: fromMaybe undefined o.maxSendHeaderBlockLength
      , paddingStrategy: fromMaybe undefined o.paddingStrategy
      , peerMaxConcurrentStreams: fromMaybe undefined o.peerMaxConcurrentStreams
      , maxSessionInvalidFrames: fromMaybe undefined o.maxSessionInvalidFrames
      , maxSessionRejectedStreams: fromMaybe undefined o.maxSessionRejectedStreams
      , settings: fromMaybe undefined o.settings
      , origins: fromMaybe undefined o.origins
      , unknownProtocolTimeout: fromMaybe undefined o.unknownProtocolTimeout
      -- tls create server options
      , "ALPNProtocols": fromMaybe undefined o."ALPNProtocols"
      , enableTrace: fromMaybe undefined o.enableTrace
      , handshakeTimeout: fromMaybe undefined o.handshakeTimeout
      , rejectUnauthorized: fromMaybe undefined o.rejectUnauthorized
      , requestCert: fromMaybe undefined o.requestCert
      , pskIdentityHint: fromMaybe undefined o.pskIdentityHint
      -- tls secure context options
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
      -- net create server options
      , allowHalfOpen: fromMaybe undefined o.allowHalfOpen
      , pauseOnConnect: fromMaybe undefined o.pauseOnConnect
      , noDelay: fromMaybe undefined o.noDelay
      , keepAlive: fromMaybe undefined o.keepAlive
      , keepAliveInitialDelay: fromMaybe undefined o.keepAliveInitialDelay
      }
  runEffectFn1 createSecureServerImpl options'

foreign import createSecureServerImpl :: EffectFn1 { | Http2CreateSecureServerOptions Unlift + TlsCreateServerOptions Unlift + TlsSecureContextOptions Unlift + NetCreateServerOptions Unlift + () } (Http2SecureServer)

-- | `port` <number>
-- | `host` <string>
-- | `path` <string> Will be ignored if port is specified. See Identifying paths for IPC connections.
-- | `backlog` <number> Common parameter of server.listen() functions.
-- | `exclusive` <boolean> Default: false
-- | `readableAll` <boolean> For IPC servers makes the pipe readable for all users. Default: false.
-- | `writableAll` <boolean> For IPC servers makes the pipe writable for all users. Default: false.
-- | `ipv6Only` <boolean> For TCP servers, setting ipv6Only to true will disable dual-stack support, i.e., binding to host :: won't make 0.0.0.0 be bound. Default: false.
type ListenOptions f =
  { port :: f Int
  , host :: f String
  , backlog :: f Number
  , exclusive :: f Boolean
  , readableAll :: f Boolean
  , writableAll :: f Boolean
  , ipv6Only :: f Boolean
  }

listen :: Http2SecureServer -> (ListenOptions Maybe -> ListenOptions Maybe) -> Effect Unit
listen s buildOptions = do
  let
    o = buildOptions
      { port: Nothing
      , host: Nothing
      , backlog: Nothing
      , exclusive: Nothing
      , readableAll: Nothing
      , writableAll: Nothing
      , ipv6Only: Nothing
      }

    finalOptions :: ListenOptions Unlift
    finalOptions =
      { port: fromMaybe undefined o.port
      , host: fromMaybe undefined o.host
      , backlog: fromMaybe undefined o.backlog
      , exclusive: fromMaybe undefined o.exclusive
      , readableAll: fromMaybe undefined o.readableAll
      , writableAll: fromMaybe undefined o.writableAll
      , ipv6Only: fromMaybe undefined o.ipv6Only
      }
  runEffectFn2 listenImpl s finalOptions

foreign import listenImpl :: EffectFn2 (Http2SecureServer) (ListenOptions Unlift) (Unit)

type Unlift :: Type -> Type
type Unlift a = a

foreign import undefined :: forall a. a

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

