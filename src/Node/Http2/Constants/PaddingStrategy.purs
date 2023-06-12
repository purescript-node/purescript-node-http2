-- | Note: not all padding strategies are listed below.
-- | I think some were added/removed in major Node versions.
module Node.Http2.Constants.PaddingStrategy where

foreign import paddingStrategyNone :: Int
foreign import paddingStrategyMax :: Int
