module Node.Http2.Constants.NGHTTP2 where

import Node.Http2.Types (ErrorCode)

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
