-- | Note: an unencrypted HTTP/2 Server (i.e. `Http2Server` in Node.js)
-- | is simply not supported in this library.
module Node.Http2.Types where

import Node.Buffer.Immutable (ImmutableBuffer)

-- | Type-level tag that indicates whether the stream/session/etc.
-- | is a server or client one.
data Endpoint

foreign import data Client :: Endpoint
foreign import data Server :: Endpoint

foreign import data Http2Session :: Endpoint -> Type

foreign import data Http2Stream :: Endpoint -> Type

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
