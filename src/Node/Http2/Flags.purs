-- | Defines a Bitwise flag and the two operations used in this context.
-- | Also groups all flags that can be used in a given frame type into
-- | a record with a corresponding name. For example,
-- | the DATA frame can use flags found in the `dataFlags` record.
module Node.Http2.Flags
  ( BitwiseFlag -- constructor intentionally not exported
  , printFlags
  , printFlags'
  , enable
  , isDisabled
  , isEnabled
  , endStream
  , ack
  , endHeaders
  , padded
  , priority
  , unFlag
  , dataFlags
  , headersFlags
  , settingsFlags
  , pushPromiseFlags
  , pingFlags
  , continuationFlags
  ) where

import Prelude

import Data.Array as Array
import Data.Int.Bits as B

-- | An integer whose bits store bitwise flags
newtype BitwiseFlag = BitwiseFlag Int

derive instance Eq BitwiseFlag
derive newtype instance Show BitwiseFlag

printFlags :: BitwiseFlag -> String
printFlags = printFlags' "; "

printFlags' :: String -> BitwiseFlag -> String
printFlags' sep input = Array.intercalate sep
  [ "END_STREAM/ACK: " <> show (isEnabled input endStream)
  , "END_STREAM: " <> show (isEnabled input endStream)
  , "END_HEADERS: " <> show (isEnabled input endHeaders)
  , "PADDED: " <> show (isEnabled input padded)
  , "PRIORITY: " <> show (isEnabled input priority)
  ]

-- | Checks whether the first arg, the `input`, has disabled the flag represented by the second arg, `endStream`.
-- | ```
-- | isDisabled input endStream
-- | ```
isDisabled :: BitwiseFlag -> BitwiseFlag -> Boolean
isDisabled (BitwiseFlag original) (BitwiseFlag flag) = original `B.and` flag == 0

-- | Checks whether the first arg, the `input`, has enabled the flag represented by the second arg, `endStream`.
-- | ```
-- | isEnabled input endStream
-- | ```
isEnabled :: BitwiseFlag -> BitwiseFlag -> Boolean
isEnabled (BitwiseFlag original) (BitwiseFlag flag) = original `B.and` flag == flag

-- | Enable two flags. 
-- | ```
-- | enable endStream padded
-- | ```
enable :: BitwiseFlag -> BitwiseFlag -> BitwiseFlag
enable (BitwiseFlag original) (BitwiseFlag flag) = BitwiseFlag (original `B.and` flag)

-- | - on DATA frames, indicates that this frame is the last that the endpoint will send for the identified stream. Setting this flag causes the stream to enter one of the "half-closed" states or the "closed" state
-- | - on HEADERS frames, indicates that the header block (Section 4.3) is the last that the endpoint will send for the identified stream.
endStream :: BitwiseFlag
endStream = BitwiseFlag 0x1

-- | - on SETTINGS frames, indicates that this frame acknowledges receipt and application of the peer's `SETTINGS` frame.  When this bit is set, the payload of the `SETTINGS` frame MUST be empty. Receipt of a SETTINGS frame with the ACK flag set and a length field value other than 0 MUST be treated as a connection error (Section 5.4.1) of type FRAME_SIZE_ERROR.  For more information, see Section 6.5.3 ("Settings Synchronization")
-- | - on PING frames, indicates that this PING frame is a PING response.  An endpoint MUST set this flag in PING responses. An endpoint MUST NOT respond to PING frames containing this flag.
ack :: BitwiseFlag
ack = BitwiseFlag 0x1

-- | - on HEADERS frames, indicates that this frame contains an entire header block (Section 4.3) and is not followed by any `CONTINUATION` frames.
-- | - on PUSH_PROMISE frames, indicates that this frame contains an entire header block (Section 4.3) and is not followed by any `CONTINUATION` frames.
-- | - on CONTINUATION frames, indicates that this frame ends a header block (Section 4.3).
endHeaders :: BitwiseFlag
endHeaders = BitwiseFlag 0x4

-- | - on DATA frames, indicates that the Pad Length field and any padding that it describes are present.
-- | - on HEADERS frames, indicates that the Pad Length field and any padding that it describes are present.
-- | - on PUSH_PROMISE frames, indicates that the Pad Length field and any padding that it describes are present.
padded :: BitwiseFlag
padded = BitwiseFlag 0x8

-- | - on HEADERS frames, indicates that the Exclusive Flag (E), Stream Dependency, and Weight fields are present; see Section 5.3.
priority :: BitwiseFlag
priority = BitwiseFlag 0x20

unFlag :: BitwiseFlag -> Int
unFlag (BitwiseFlag i) = i

dataFlags :: { endStream :: BitwiseFlag, padded :: BitwiseFlag }
dataFlags =
  { endStream
  , padded
  }

headersFlags :: { endHeaders :: BitwiseFlag, endStream :: BitwiseFlag, padded :: BitwiseFlag, priority :: BitwiseFlag }
headersFlags =
  { endStream
  , endHeaders
  , padded
  , priority
  }

settingsFlags :: { ack :: BitwiseFlag }
settingsFlags =
  { ack
  }

pushPromiseFlags :: { endHeaders :: BitwiseFlag, padded :: BitwiseFlag }
pushPromiseFlags =
  { endHeaders
  , padded
  }

pingFlags :: { ack :: BitwiseFlag }
pingFlags =
  { ack
  }

continuationFlags :: { endHeaders :: BitwiseFlag }
continuationFlags =
  { endHeaders
  }
