{-# LANGUAGE OverloadedStrings #-}
module QB.Seed where

import Data.ByteString (ByteString)
import Data.Yaml

import QB.Types (FmrType)

data Seed = Seed
  { scheme :: String
  , axes :: [String]
  , bases :: [String]
  , elemType :: FmrType
  , eomRank :: Int -- 時間1階微分を計算するのに必要な空間微分の階数の最大値
  , withFilter :: Bool
  , params :: Maybe String
  , initialCondition :: Maybe String
  , eom :: Maybe String
  } deriving (Show,Eq)

instance FromJSON Seed where
  parseJSON (Object v) =
    Seed <$> v .: "scheme"
         <*> v .: "axes"
         <*> v .: "bases"
         <*> v .: "elem-type"
         <*> v .: "eom-rank"
         <*> v .: "with-filter"
         <*> v .:? "params"
         <*> v .:? "initial-condition"
         <*> v .:? "eom"
  parseJSON _ = fail "Expected Object for Seed value"

getSeed :: FilePath -> IO Seed
getSeed = decodeFileThrow

decodeSeed :: ByteString -> Maybe Seed
decodeSeed xs = case decodeEither' xs of
                  Right s -> Just s
                  Left _ -> Nothing
