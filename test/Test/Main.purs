module Test.Main where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (forWithIndex_, traverseWithIndex_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Foreign.Object (Object)
import Node.Buffer as Buffer
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Http2.Client as Client
import Node.Http2.Constants as NGHTTP2
import Node.Http2.Server as Server
import Node.Http2.Session as Session
import Node.Http2.Stream (session, toDuplex)
import Node.Http2.Stream as H2Stream
import Node.Path as Path
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

unsafeToImmutableBuffer :: Buffer.Buffer -> Effect ImmutableBuffer
unsafeToImmutableBuffer = Buffer.unsafeFreeze

main :: Effect Unit
main = do
  privateKey <- FS.readFile (Path.concat [ "test", "localhost-privkey.pem" ]) >>= unsafeToImmutableBuffer
  cert <- FS.readFile (Path.concat [ "test", "localhost-cert.pem" ]) >>= unsafeToImmutableBuffer
  server <- Server.createSecureServer
    ( _
        { key = Just [ privateKey ]
        , cert = Just [ cert ]
        }
    )
  Server.onCheckContinue server \req res -> do
    log "server - onCheckContinue"
  Server.onConnection server \duplex -> do
    log "server - onConnection"
  Server.onSession server \session -> do
    log "server - onSession"
  Server.onSessionError server \err session -> do
    log "server - onSessionError"
    log (unsafeCoerce err)
  Server.onStream server \stream headers flags rawHeaders -> do
    streamId <- H2Stream.id stream
    log $ "server - onStream for id: " <> show streamId
    forWithIndex_ (unsafeCoerce headers :: Object String) \k v ->
      log $ k <> ": " <> v
    log $ "server - onStream - Flags: " <> show flags
    log $ "server - onStream - Raw Headers: " <> show rawHeaders
    let duplex = H2Stream.toDuplex stream
    H2Stream.respond stream (unsafeCoerce { "an-http-header": "value" })
      { endStream: true
      , waitForTrailers: false
      }
    void $ Stream.writeString duplex UTF8 "hello from server" (const mempty)
    void $ Stream.end duplex \_ -> do
      H2Stream.close stream NGHTTP2.noError

  Server.onTimeout server do
    log "onTimeout"
  Server.onUnknownProtocol server \duplex -> do
    log "onUnknownProtocol"
  let httpsPort = 443
  Server.listen server
    ( _
        { port = Just httpsPort
        }
    )
  session <- Client.connect' ("https://localhost:" <> show httpsPort)
    (_ { ca = Just [ cert ] })
  Session.onError session \error ->
    log $ "Client session encountered error: " <> Exception.message error
  stream <- Session.request session
    ( unsafeCoerce
        { ":method": "GET"
        , ":path": "/"
        }
    )
  let duplex = toDuplex stream
  void $ Stream.end duplex (const mempty)
  H2Stream.onResponse stream \headers flags -> do
    log "client - onResponse"
    forWithIndex_ (unsafeCoerce headers :: Object String) \k v ->
      log $ k <> ": " <> v
    log $ "Flags: " <> show flags
    chunksRef <- Ref.new []
    Stream.onData duplex \buf ->
      Ref.modify_ (flip Array.snoc buf) chunksRef
    Stream.onEnd duplex do
      chunks <- Ref.read chunksRef
      buffer <- Buffer.concat chunks :: Effect Buffer.Buffer
      str <- Buffer.toString UTF8 buffer :: Effect String
      log $ "client - onResponse body: " <> show str
      H2Stream.close stream NGHTTP2.noError

  Server.close server
