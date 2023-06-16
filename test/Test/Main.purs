module Test.Main where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (forWithIndex_)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Foreign.Object (Object)
import Node.Buffer as Buffer
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on)
import Node.FS.Sync as FS
import Node.Http2.Client as Client
import Node.Http2.Constants.NGHTTP2 as NGHTTP2
import Node.Http2.Server as Server
import Node.Http2.Session as Session
import Node.Http2.Stream (toDuplex)
import Node.Http2.Stream as H2Stream
import Node.Http2.Types (Http2Session)
import Node.Net.Server as NServer
import Node.Path as Path
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

unsafeToImmutableBuffer :: Buffer.Buffer -> Effect ImmutableBuffer
unsafeToImmutableBuffer = Buffer.unsafeFreeze

logWith :: String -> String -> Effect Unit
logWith msg x = log $ msg <> ": " <> x

logShowWith :: forall a. Show a => String -> a -> Effect Unit
logShowWith msg = logWith msg <<< show

main :: Effect Unit
main = do
  privateKey <- FS.readFile (Path.concat [ "test", "localhost-privkey.pem" ])
  cert <- FS.readFile (Path.concat [ "test", "localhost-cert.pem" ])
  server <- Server.createSecureServer
    { key: [ privateKey ]
    , cert: [ cert ]
    }
  on Server.checkContinueHandle server.http2 \req res -> do
    log "server - onCheckContinue"
  on NServer.connectionHandle server.net \duplex -> do
    log "server - onConnection"
  on Server.sessionHandle server.http2 \session -> do
    log "server - onSession"
    log "Testing properties for any thrown errors"
    printHttp2SessionState session

  on Server.sessionErrorHandle server.http2 \err session -> do
    log "server - onSessionError"
    log (unsafeCoerce err)
    printHttp2SessionState session
  on Server.streamHandle server.http2 \stream headers flags rawHeaders -> do
    streamId <- H2Stream.id stream
    log $ "server - onStream for id: " <> show streamId
    forWithIndex_ (unsafeCoerce headers :: Object String) \k v ->
      log $ k <> ": " <> v
    log $ "server - onStream - Flags: " <> show flags
    log $ "server - onStream - Raw Headers: " <> show rawHeaders
    let duplex = H2Stream.toDuplex stream
    H2Stream.respond stream (unsafeCoerce { "an-http-header": "value" })
      { endStream: false -- if this is true, then Stream.write/end produces error
      , waitForTrailers: true
      }
    -- H2Stream.close stream NGHTTP2.noError
    Stream.writeStringCb_ duplex UTF8 "hello from server" \err -> do
      log (unsafeCoerce err)
      Stream.end' duplex \_ -> do
        log $ "server - onStream - closing for id: " <> show streamId
        H2Stream.close stream NGHTTP2.noError

  on Server.timeoutHandle server.tls do
    log "onTimeout"
  on Server.unknownProtocolHandle server.http2 \duplex -> do
    log "onUnknownProtocol"
  -- https://stackoverflow.com/a/63173619
  -- "In UNIX-like systems, non-root users are unable to bind to ports lower than 1024."
  let httpsPort = 8443
  NServer.listenTcp server.net
    { port: httpsPort
    }
  on NServer.listeningHandle server.net do
    log "server listening"
    session <- Client.connect' ("https://localhost:" <> show httpsPort)
      { ca: [ cert ]
      }
    on Session.errorHandle session \error ->
      log $ "Client session encountered error: " <> Exception.message error
    stream <- Session.request session
      ( unsafeCoerce
          { ":method": "GET"
          , ":path": "/"
          }
      )
    let duplex = toDuplex stream
    Stream.end duplex
    H2Stream.onResponse stream \headers flags -> do
      log "client - onResponse"
      forWithIndex_ (unsafeCoerce headers :: Object String) \k v ->
        log $ k <> ": " <> v
      log $ "Flags: " <> show flags
      chunksRef <- Ref.new []
      on Stream.dataHandle duplex \buf ->
        Ref.modify_ (flip Array.snoc buf) chunksRef
      on Stream.endHandle duplex do
        chunks <- Ref.read chunksRef
        buffer <- Buffer.concat chunks :: Effect Buffer.Buffer
        str <- Buffer.toString UTF8 buffer :: Effect String
        log $ "client - onResponse body: " <> show str
        H2Stream.close stream NGHTTP2.noError
        Session.destroy session
        NServer.close server.net

printHttp2SessionState :: forall endpoint. Http2Session endpoint -> Effect Unit
printHttp2SessionState session = do
  Session.alpnProtocol session >>= logShowWith "ALPN Protocol"
  Session.closed session >>= logShowWith "closed"
  Session.connecting session >>= logShowWith "connecting"
  Session.destroyed session >>= logShowWith "destroyed"
  Session.encrypted session >>= logShowWith "encrypted"
  logShowWith "origin set" $ Session.originSet session
  Session.pendingSettingsAck session >>= logShowWith "pending settings acknowlegement"
  Session.remoteSettings session >>= logShowWith "remote settings"
  Session.state session >>= logShowWith "state"
  logShowWith "type" $ Session.type_ session

