module Test.Main where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on_)
import Node.FS.Sync as FS
import Node.Http2.Client as Client
import Node.Http2.ErrorCode as ErrorCode
import Node.Http2.Flags (printFlags)
import Node.Http2.Headers (printHeaders')
import Node.Http2.Server as Server
import Node.Http2.Session as Session
import Node.Http2.Stream (toDuplex)
import Node.Http2.Stream as H2Stream
import Node.Http2.Types (Http2Session)
import Node.Net.Server as NServer
import Node.Path as Path
import Node.Stream as Stream
import Node.TLS.Server as Tls
import Unsafe.Coerce (unsafeCoerce)

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
  let
    tlsServer = Server.toTlsServer server
    tcpServer = Tls.toTcpServer tlsServer
  server # on_ Server.checkContinueH \req res -> do
    log "server - onCheckContinue"
  tcpServer # on_ NServer.connectionH \duplex -> do
    log "server - onConnection"
  server # on_ Server.sessionH \session -> do
    log "server - onSession"
    log "Testing properties for any thrown errors"
    printHttp2SessionState session

  server # on_ Server.sessionErrorH \err session -> do
    log "server - onSessionError"
    log (unsafeCoerce err)
    printHttp2SessionState session
  server # on_ Server.streamH \stream headers flags rawHeaders -> do
    streamId <- H2Stream.id stream
    log $ "server - onStream for id: " <> show streamId
    log $ printHeaders' "\n" headers
    log $ "server - onStream - Flags: " <> printFlags flags
    log $ "server - onStream - Raw Headers: " <> show rawHeaders
    let duplex = H2Stream.toDuplex stream
    H2Stream.respond stream (unsafeCoerce { "an-http-header": "value" })
      { endStream: false -- if this is true, then Stream.write/end produces error
      , waitForTrailers: true
      }
    -- H2Stream.close stream ErrorCode.noError
    void $ Stream.writeString' duplex UTF8 "hello from server" \err -> do
      log (unsafeCoerce err)
      Stream.end' duplex \_ -> do
        log $ "server - onStream - closing for id: " <> show streamId
        H2Stream.close stream ErrorCode.noError

  tlsServer # on_ Server.timeoutH do
    log "onTimeout"
  server # on_ Server.unknownProtocolH \duplex -> do
    log "onUnknownProtocol"
  -- https://stackoverflow.com/a/63173619
  -- "In UNIX-like systems, non-root users are unable to bind to ports lower than 1024."
  let httpsPort = 8443
  NServer.listenTcp tcpServer
    { port: httpsPort
    }
  tcpServer # on_ NServer.listeningH do
    log "server listening"
    session <- Client.connect' ("https://localhost:" <> show httpsPort)
      { ca: [ cert ]
      }
    session # on_ Session.errorH \error ->
      log $ "Client session encountered error: " <> Exception.message error
    stream <- Session.request session
      ( unsafeCoerce
          { ":method": "GET"
          , ":path": "/"
          }
      )
    let duplex = toDuplex stream
    Stream.end duplex
    stream # on_ H2Stream.responseH \headers flags -> do
      log "client - onResponse"
      log $ printHeaders' "\n" headers
      log $ "Flags: " <> printFlags flags
      chunksRef <- Ref.new []
      duplex # on_ Stream.dataH \buf ->
        Ref.modify_ (flip Array.snoc buf) chunksRef
      duplex # on_ Stream.endH do
        chunks <- Ref.read chunksRef
        buffer <- Buffer.concat chunks :: Effect Buffer.Buffer
        str <- Buffer.toString UTF8 buffer :: Effect String
        log $ "client - onResponse body: " <> show str
        H2Stream.close stream ErrorCode.noError
        Session.destroy session
        NServer.close tcpServer

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

