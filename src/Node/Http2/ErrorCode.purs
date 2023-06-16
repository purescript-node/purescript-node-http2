module Node.Http2.ErrorCode where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

newtype ErrorCode = ErrorCode Int

derive instance Eq ErrorCode
derive instance Ord ErrorCode
derive instance Newtype ErrorCode _
derive instance Generic ErrorCode _
derive newtype instance Show ErrorCode

foreign import noError :: ErrorCode
foreign import protocolError :: ErrorCode
foreign import internalError :: ErrorCode
foreign import flowControlError :: ErrorCode
foreign import settingsTimeout :: ErrorCode
foreign import streamClosed :: ErrorCode
foreign import frameSizeError :: ErrorCode
foreign import refusedStream :: ErrorCode
foreign import cancel :: ErrorCode
foreign import compressionError :: ErrorCode
foreign import connectError :: ErrorCode
foreign import enhanceYourCalm :: ErrorCode
foreign import inadequateSecurity :: ErrorCode
foreign import http1_1Required :: ErrorCode
