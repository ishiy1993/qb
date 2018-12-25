module QB.Scheme where

import qualified Data.ByteString.Builder as B
import Data.Maybe (fromMaybe)

import QB.Types
import QB.Seed (Seed)
import qualified QB.Seed as S
import QB.Scheme.SL4TH3

generateCodeStructure :: Seed -> CodeStructure
generateCodeStructure s | S.scheme s == "sl4th3" = withSL4TH3 s
                        | otherwise = withDefault s

withDefault :: Seed -> CodeStructure
withDefault s = CodeStructure { target = S.scheme s
                              , axes = map B.stringUtf8 $ S.axes s
                              , gridStruct = map B.stringUtf8 $ S.bases s
                              , elemType = B.stringUtf8 $ S.elemType s
                              , defs = map B.stringUtf8 ["#PARAMS",fromMaybe "" (S.params s),"#INIT",fromMaybe "" (S.initialCondition s)]
                              , initBody = []
                              , firstStepBody = Nothing
                              , filterBody = if S.withFilter s then Just [] else Nothing
                              , stepBody = []
                              }
