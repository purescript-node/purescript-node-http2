module Node.Http2.FrameType where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

newtype FrameType = FrameType Int

derive instance Eq FrameType
derive instance Ord FrameType
derive instance Newtype FrameType _
derive instance Generic FrameType _
derive newtype instance Show FrameType

frameData :: FrameType
frameData = FrameType 0x0

frameHeaders :: FrameType
frameHeaders = FrameType 0x1

framePriority :: FrameType
framePriority = FrameType 0x2

frameRstStream :: FrameType
frameRstStream = FrameType 0x3

frameSettings :: FrameType
frameSettings = FrameType 0x4

framePushPromise :: FrameType
framePushPromise = FrameType 0x5

framePing :: FrameType
framePing = FrameType 0x6

frameGoAway :: FrameType
frameGoAway = FrameType 0x7

frameWindowUpdate :: FrameType
frameWindowUpdate = FrameType 0x8

frameContinuation :: FrameType
frameContinuation = FrameType 0x9
