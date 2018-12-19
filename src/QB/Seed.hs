module QB.Seed where

import QB.Types (FmrType)

data Seed = Seed
  { scheme :: String
  , axes :: [String]
  , bases :: [String]
  , elemType :: FmrType
  , eomRank :: Int -- 時間1階微分を計算するのに必要な空間微分の階数の最大値
  , withFilter :: Bool
  }
