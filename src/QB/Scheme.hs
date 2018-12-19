module QB.Scheme where

import QB.Types
import QB.Seed (Seed)
import qualified QB.Seed as S
import QB.Scheme.SL4TH3

generateCodeStructure :: Seed -> CodeStructure
generateCodeStructure s | S.scheme s == "sl4th3" = withSL4TH3 s
                        | otherwise = withDefault s

withDefault :: Seed -> CodeStructure
withDefault s = CodeStructure { target = S.scheme s
                              , axes = S.axes s
                              , gridStruct = S.bases s
                              , elemType = S.elemType s
                              , defs = []
                              , initBody = []
                              , firstStepBody = Nothing
                              , filterBody = if S.withFilter s then Just [] else Nothing
                              , stepBody = []
                              }
