-- | Note: not all padding strategies are listed below.
-- | I think some were added/removed in major Node versions.
module Node.Http2.PaddingStrategy where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

newtype PaddingStrategy = PaddingStrategy Int

derive instance Eq PaddingStrategy
derive instance Ord PaddingStrategy
derive instance Newtype PaddingStrategy _
derive instance Generic PaddingStrategy _
derive newtype instance Show PaddingStrategy

foreign import paddingStrategyNone :: Int
foreign import paddingStrategyMax :: Int
