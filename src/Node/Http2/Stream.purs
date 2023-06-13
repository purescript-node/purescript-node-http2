module Node.Http2.Stream
  ( onAbort
  , onClose
  , onError
  , onFrameError
  , onReady
  , onTimeout
  , onTrailers
  , onWantTrailers
  , bufferSize
  , close
  , closed
  , destroyed
  , endAfterHeaders
  , id
  , pending
  , PriorityOptions
  , priority
  , rstCode
  , sentHeaders
  , sentInfoHeaders
  , sentTrailers
  , session
  , setTimeout
  , Http2StreamState
  , state
  , sendTrailers
  , additionalHeaders
  , headersSent
  , pushAllowed
  , PushStreamOptions
  , pushStream
  , pushStream'
  , RespondOptions
  , respond
  , RespondWithFdOptions
  , respondWithFd
  , RespondWithFileOptions
  , respondWithFile
  , onContinue
  , onHeaders
  , onPush
  , onResponse
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Node.FS (FileDescriptor)
import Node.Http2.Types (Client, Headers, Http2Session, Http2Stream, Server)
import Node.Path (FilePath)
import Partial.Unsafe (unsafeCrashWith)

onAbort :: forall peer. Http2Stream peer -> Effect Unit -> Effect Unit
onAbort s cb = runEffectFn2 onAbortImpl s cb

foreign import onAbortImpl :: forall peer. EffectFn2 (Http2Stream peer) (Effect Unit) Unit

onClose :: forall peer. Http2Stream peer -> Effect Unit -> Effect Unit
onClose s cb = runEffectFn2 onCloseImpl s cb

foreign import onCloseImpl :: forall peer. EffectFn2 (Http2Stream peer) (Effect Unit) (Unit)

onError :: forall peer. Http2Stream peer -> (Error -> Effect Unit) -> Effect Unit
onError s cb = runEffectFn2 onErrorImpl s $ mkEffectFn1 cb

foreign import onErrorImpl :: forall peer. EffectFn2 (Http2Stream peer) (EffectFn1 Error Unit) (Unit)

onFrameError :: forall peer. Http2Stream peer -> (Int -> Int -> Int -> Effect Unit) -> Effect Unit
onFrameError s cb = runEffectFn2 onFrameErrorImpl s $ mkEffectFn3 cb

foreign import onFrameErrorImpl :: forall peer. EffectFn2 (Http2Stream peer) (EffectFn3 Int Int Int Unit) (Unit)

onReady :: forall peer. Http2Stream peer -> Effect Unit -> Effect Unit
onReady s cb = runEffectFn2 onReadyImpl s cb

foreign import onReadyImpl :: forall peer. EffectFn2 (Http2Stream peer) (Effect Unit) (Unit)

onTimeout :: forall peer. Http2Stream peer -> Effect Unit -> Effect Unit
onTimeout s cb = runEffectFn2 onTimeoutImpl s cb

foreign import onTimeoutImpl :: forall peer. EffectFn2 (Http2Stream peer) (Effect Unit) (Unit)

onTrailers :: forall peer. Http2Stream peer -> (Headers -> Int -> Effect Unit) -> Effect Unit
onTrailers s cb = runEffectFn2 onTrailersImpl s $ mkEffectFn2 cb

foreign import onTrailersImpl :: forall peer. EffectFn2 (Http2Stream peer) (EffectFn2 Headers Int Unit) (Unit)

onWantTrailers :: forall peer. Http2Stream peer -> Effect Unit -> Effect Unit
onWantTrailers s cb = runEffectFn2 onWantTrailersImpl s cb

foreign import onWantTrailersImpl :: forall peer. EffectFn2 (Http2Stream peer) (Effect Unit) (Unit)

bufferSize :: forall peer. Http2Stream peer -> Effect Int
bufferSize s = runEffectFn1 bufferSizeImpl s

foreign import bufferSizeImpl :: forall peer. EffectFn1 (Http2Stream peer) (Int)

close :: forall peer. Http2Stream peer -> Int -> Effect Unit
close s code = runEffectFn2 closeImpl s code

foreign import closeImpl :: forall peer. EffectFn2 (Http2Stream peer) Int (Unit)

closed :: forall peer. Http2Stream peer -> Effect Boolean
closed s = runEffectFn1 closedImpl s

foreign import closedImpl :: forall peer. EffectFn1 (Http2Stream peer) (Boolean)

destroyed :: forall peer. Http2Stream peer -> Effect Boolean
destroyed s = runEffectFn1 destroyedImpl s

foreign import destroyedImpl :: forall peer. EffectFn1 (Http2Stream peer) (Boolean)

endAfterHeaders :: forall peer. Http2Stream peer -> Effect Boolean
endAfterHeaders s = runEffectFn1 endAfterHeadersImpl s

foreign import endAfterHeadersImpl :: forall peer. EffectFn1 (Http2Stream peer) (Boolean)

id :: forall peer. Http2Stream peer -> Effect (Maybe Int)
id s = map toMaybe $ runEffectFn1 idImpl s

foreign import idImpl :: forall peer. EffectFn1 (Http2Stream peer) (Nullable Int)

pending :: forall peer. Http2Stream peer -> Effect Boolean
pending s = runEffectFn1 pendingImpl s

foreign import pendingImpl :: forall peer. EffectFn1 (Http2Stream peer) (Boolean)

-- | `exclusive` <boolean> When true and parent identifies a parent Stream, this stream is made the sole direct dependency of the parent, with all other existing dependents made a dependent of this stream. Default: false.
-- | `parent` <number> Specifies the numeric identifier of a stream this stream is dependent on.
-- | `weight` <number> Specifies the relative dependency of a stream in relation to other streams with the same parent. The value is a number between 1 and 256 (inclusive).
-- | `silent` <boolean> When true, changes the priority locally without sending a PRIORITY frame to the connected peer.
type PriorityOptions =
  { exclusive :: Boolean
  , parent :: Int
  , weight :: Int
  , silent :: Boolean
  }

-- | Note: clamping the `weight` value is done automatically.
priority :: forall peer. Http2Stream peer -> PriorityOptions -> Effect Unit
priority s p = runEffectFn2 priorityImpl s $ p { weight = clamp 1 256 p.weight }

foreign import priorityImpl :: forall peer. EffectFn2 (Http2Stream peer) (PriorityOptions) (Unit)

rstCode :: forall peer. Http2Stream peer -> Effect (Maybe Int)
rstCode s = map toMaybe $ runEffectFn1 rstCodeImpl s

foreign import rstCodeImpl :: forall peer. EffectFn1 (Http2Stream peer) (Nullable Int)

sentHeaders :: forall peer. Http2Stream peer -> Effect Headers
sentHeaders s = runEffectFn1 sentHeadersImpl s

foreign import sentHeadersImpl :: forall peer. EffectFn1 (Http2Stream peer) (Headers)

sentInfoHeaders :: forall peer. Http2Stream peer -> Effect (Array Headers)
sentInfoHeaders s = runEffectFn1 sentInfoHeadersImpl s

foreign import sentInfoHeadersImpl :: forall peer. EffectFn1 (Http2Stream peer) (Array Headers)

sentTrailers :: forall peer. Http2Stream peer -> Effect Headers
sentTrailers s = runEffectFn1 sentTrailersImpl s

foreign import sentTrailersImpl :: forall peer. EffectFn1 (Http2Stream peer) (Headers)

session :: forall peer. Http2Stream peer -> Effect (Maybe (Http2Session peer))
session s = map toMaybe $ runEffectFn1 sessionImpl s

foreign import sessionImpl :: forall peer. EffectFn1 (Http2Stream peer) (Nullable (Http2Session peer))

setTimeout :: forall peer. Http2Stream peer -> Milliseconds -> Effect Unit -> Effect Unit
setTimeout s msecs cb = runEffectFn3 setTimeoutImpl s msecs cb

foreign import setTimeoutImpl :: forall peer. EffectFn3 (Http2Stream peer) (Milliseconds) (Effect Unit) (Unit)

-- | `localWindowSize` <number> The number of bytes the connected peer may send for this Http2Stream peer without receiving a WINDOW_UPDATE.
-- | `state` <number> A flag indicating the low-level current state of the Http2Stream peer as determined by nghttp2.
-- | `localClose` <number> 1 if this Http2Stream peer has been closed locally.
-- | `remoteClose` <number> 1 if this Http2Stream peer has been closed remotely.
-- | `sumDependencyWeight` <number> The sum weight of all Http2Stream peer instances that depend on this Http2Stream peer as specified using PRIORITY frames.
-- | `weight` <number> The priority weight of this Http2Stream peer.
type Http2StreamState =
  { localWindowSize :: Int
  , state :: Int
  , localClose :: Int
  , remoteClose :: Int
  , sumDependencyWeight :: Int
  , weight :: Int
  }

state :: forall peer. Http2Stream peer -> Effect Http2StreamState
state s = runEffectFn1 stateImpl s

foreign import stateImpl :: forall peer. EffectFn1 (Http2Stream peer) (Http2StreamState)

sendTrailers :: forall peer. Http2Stream peer -> Headers -> Effect Unit
sendTrailers s t = runEffectFn2 sendTrailersImpl s t

foreign import sendTrailersImpl :: forall peer. EffectFn2 (Http2Stream peer) (Headers) (Unit)

additionalHeaders :: Http2Stream Server -> Headers -> Effect Unit
additionalHeaders s h = runEffectFn2 additionalHeadersImpl s h

foreign import additionalHeadersImpl :: EffectFn2 (Http2Stream Server) (Headers) (Unit)

headersSent :: Http2Stream Server -> Effect Boolean
headersSent s = runEffectFn1 headersSentImpl s

foreign import headersSentImpl :: EffectFn1 (Http2Stream Server) (Boolean)

pushAllowed :: Http2Stream Server -> Effect Boolean
pushAllowed s = runEffectFn1 pushAllowedImpl s

foreign import pushAllowedImpl :: EffectFn1 (Http2Stream Server) (Boolean)

-- | `exclusive` <boolean> When true and parent identifies a parent Stream, the created stream is made the sole direct dependency of the parent, with all other existing dependents made a dependent of the newly created stream. Default: false.
-- | `parent` <number> Specifies the numeric identifier of a stream the newly created stream is dependent on.
type PushStreamOptions =
  { exclusive :: Boolean
  , parent :: Int
  }

pushStream :: Http2Stream Server -> Headers -> (Either Error (Http2Stream Server) -> Headers -> Effect Unit) -> Effect Unit
pushStream s h cb = runEffectFn3 pushStreamNoOptionsImpl s h $ mkEffectFn3 \err strm hdrs ->
  case toMaybe err, toMaybe strm of
    Just e, _ -> cb (Left e) hdrs
    _, Just stm -> cb (Right stm) hdrs
    _, _ -> unsafeCrashWith "Impossible: one must be `Just`"

foreign import pushStreamNoOptionsImpl :: EffectFn3 (Http2Stream Server) (Headers) (EffectFn3 (Nullable Error) (Nullable (Http2Stream Server)) Headers Unit) Unit

pushStream' :: Http2Stream Server -> Headers -> PushStreamOptions -> (Either Error (Http2Stream Server) -> Headers -> Effect Unit) -> Effect Unit
pushStream' s h opt cb = runEffectFn4 pushStreamOptionsImpl s h opt $ mkEffectFn3 \err strm hdrs ->
  case toMaybe err, toMaybe strm of
    Just e, _ -> cb (Left e) hdrs
    _, Just stm -> cb (Right stm) hdrs
    _, _ -> unsafeCrashWith "Impossible: one must be `Just`"

foreign import pushStreamOptionsImpl :: EffectFn4 (Http2Stream Server) (Headers) (PushStreamOptions) (EffectFn3 (Nullable Error) (Nullable (Http2Stream Server)) Headers Unit) Unit

-- | `endStream` <boolean> Set to true to indicate that the response will not include payload data.
-- | `waitForTailers` <boolean> When true, the Http2Stream will emit the 'wantTrailers' event after the final DATA frame has been sent.
type RespondOptions =
  { endStream :: Boolean
  , waitForTrailers :: Boolean
  }

respond :: Http2Stream Server -> Headers -> RespondOptions -> Effect Unit
respond s h o = runEffectFn3 respondImpl s h o

foreign import respondImpl :: EffectFn3 (Http2Stream Server) (Headers) (RespondOptions) (Unit)

-- | `waitForTrailers` <boolean> When true, the Http2Stream will emit the 'wantTrailers' event after the final DATA frame has been sent.
-- | `offset` <number> The offset position at which to begin reading.
-- | `length` <number> The amount of data from the fd to send.
-- |
-- | Note: `statCheck` function intentionally not supported.
type RespondWithFdOptions =
  { waitForTrailers :: Boolean
  , offset :: Int
  , length :: Int
  }

respondWithFd :: Http2Stream Server -> FileDescriptor -> Headers -> RespondWithFdOptions -> Effect Unit
respondWithFd s fd h o = runEffectFn4 respondWithFdImpl s fd h o

foreign import respondWithFdImpl :: EffectFn4 (Http2Stream Server) (FileDescriptor) (Headers) (RespondWithFdOptions) (Unit)

-- | `waitForTrailers` <boolean> When true, the Http2Stream will emit the 'wantTrailers' event after the final DATA frame has been sent.
-- | `offset` <number> The offset position at which to begin reading.
-- | `length` <number> The amount of data from the fd to send.
-- | 
-- | Note: `statCheck` and `onError` intentionally not supported.
type RespondWithFileOptions =
  { waitForTrailers :: Boolean
  , offset :: Int
  , length :: Int
  }

respondWithFile :: Http2Stream Server -> FilePath -> Headers -> RespondWithFileOptions -> Effect Unit
respondWithFile s fp h o = runEffectFn4 respondWithFileImpl s fp h o

foreign import respondWithFileImpl :: EffectFn4 (Http2Stream Server) (FilePath) (Headers) (RespondWithFileOptions) (Unit)

onContinue :: Http2Stream Client -> Effect Unit -> Effect Unit
onContinue s cb = runEffectFn2 onContinueImpl s cb

foreign import onContinueImpl :: EffectFn2 (Http2Stream Client) (Effect Unit) (Unit)

onHeaders :: Http2Stream Client -> (Headers -> Int -> Effect Unit) -> Effect Unit
onHeaders s cb = runEffectFn2 onHeadersImpl s $ mkEffectFn2 cb

foreign import onHeadersImpl :: EffectFn2 (Http2Stream Client) (EffectFn2 Headers Int Unit) (Unit)

onPush :: Http2Stream Client -> (Headers -> Int -> Effect Unit) -> Effect Unit
onPush s cb = runEffectFn2 onPushImpl s $ mkEffectFn2 cb

foreign import onPushImpl :: EffectFn2 (Http2Stream Client) (EffectFn2 Headers Int Unit) (Unit)

onResponse :: Http2Stream Client -> (Headers -> Int -> Effect Unit) -> Effect Unit
onResponse s cb = runEffectFn2 onResponseImpl s $ mkEffectFn2 cb

foreign import onResponseImpl :: EffectFn2 (Http2Stream Client) (EffectFn2 Headers Int Unit) (Unit)

