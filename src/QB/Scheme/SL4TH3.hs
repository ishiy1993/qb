{-# LANGUAGE OverloadedLabels #-}
module QB.Scheme.SL4TH3 (withSL4TH3) where

import Data.List (intercalate, nub, sort, group, findIndex)
import Data.Maybe (fromJust, fromMaybe)
import Text.Printf

import QB.Types
import QB.Seed (Seed)
import qualified QB.Seed as S

-- もうちょっと数値の扱いを改善したい
-- +1 -> +
-- 0 -> ""
-- など
mkDiff :: (Int,Int,String,Int) -> String
mkDiff (dim,i,a,o)
  | o == 1 = build [(-2,1),(-1,-8),(1,8),(2,-1)] ("12*d" ++ a)
  | o == 2 = build [(-2,-1),(-1,16),(0,-30),(1,16),(2,-1)] ("12*d" ++ a ++ "**2")
  | o == 3 = build [(-2,-1),(-1,2),(1,-2),(2,1)] ("2*d" ++ a ++ "**3")
  | o == 4 = build [(-2,1),(-1,-4),(0,6),(1,-4),(2,1)] ("d" ++ a ++ "**4")
  | otherwise = error "Error in mkDiff"
  where
    mkIdx s = "q" ++ bckt ["i" ++ show j ++ if j == i then printf "%+d" s else "" | j <- [1..dim]]
    build :: [(Int,Int)] -> String -> String
    build tbl c0 = printf "(%s)/(%s)" (unwords [printf "%+d*" c ++ mkIdx s | (s,c) <- tbl]) c0

mkSmooth :: (Int,Int) -> String
mkSmooth (dim,i) = printf "11*q[%s]/16 + 15*(q[%s] + q[%s])/64 - 3*(q[%s] + q[%s])/32 + (q[%s] + q[%s])/64" (mkIdx 0) (mkIdx 1) (mkIdx (-1)) (mkIdx 2) (mkIdx (-2)) (mkIdx 3) (mkIdx (-3)) 
  where
    mkIdx :: Int -> String
    mkIdx s = intercalate "," ["i" ++ show j ++ if j == i then printf "%+d" s else "" | j <- [1..dim]]

withSL4TH3 :: Seed -> CodeStructure
withSL4TH3 s = CodeStructure { target = S.scheme s
                             , axes = S.axes s
                             , gridStruct = [q ++ x | x <- ["c","p","h"], q <- qs]
                             , elemType = S.elemType s
                             , defs = ds
                             , initBody = ib
                             , firstStepBody = Just fsb
                             , filterBody = if S.withFilter s then Just flb else Nothing
                             , stepBody = sb
                             }
  where
    as = S.axes s
    dim = length as
    qs = S.bases s
    [qc,qp,qh] = [map (++x) | x <- ["c","p","h"]] <*> [qs]
    [qc',qp',qh'] = map (++"'") <$> [qc,qp,qh]
    -- TODO: Refactoring ↓
    delIdx = tail $ concat $ map (nub . map sort) $ scanl (\acc a -> (:) <$> a <*> acc) [[]] (replicate (2*S.eomRank s) as)
    isPure = (==1) . length . group
    pures = filter isPure delIdx
    cross = filter (not . isPure) delIdx
    mkD i = "d_" ++ concat i
    diffs = unlines $ ["#DIFF"]
                    ++ [mkD d <=> "fun(q) " ++ mkDiff (dim, 1 + fromJust (findIndex (==a) as),a,length d) | d <- pures, let a = head d]
                    ++ [mkD d <=> intercalate " . " (map mkD $ group d) | d <- cross]
    smooth = unlines $ [ "#SMOOTH" ]
                     ++ ["smooth_" ++ a <=> "fun(q) " ++ mkSmooth (dim,i) | (i,a) <- zip [1..dim] as]
                     ++ ["smooth" <=> intercalate " . " ["smooth_" ++ a | a <- as]]
    updateF = defFun "update" "(qc,q_t,q_tt)" "(qc',qp',qh')"
                [ "qc' = qc"
                , "qp' = qc + dt*q_t + dt*dt*q_tt/2"
                , "qh' = qc + dt*q_t/2 + dt*dt*q_tt/12"
                ]
    delF = defFun "del" "(q)" "(q_t,q_tt)" $
            [ qs =@ "q" ]
            ++ [ (map (++"_"++d) qs) =@ ("d_"++ d ++ " q") | d0 <- delIdx, let d = concat d0 ] ++
            [ "#EOM"
            , ""
            , "q_t" @= map (++"_t") qs
            , "q_tt" @= map (++"_tt") qs
            ]
    ds = ["#PARAM", fromMaybe "" (S.params s), "#INIT", fromMaybe "" (S.initialCondition s), diffs, smooth, updateF, delF]
    ib = [q ++ "c" ++ bckt ["i" ++ show i | i <- [1..dim]] <=> "initialize_" ++ q ++ paren ["i" ++ show i ++ "*d" ++ a | (i,a) <- zip [1..dim] as] | q <- qs]
    fsb = [ "q" @= qc
          , "(q_t,q_tt) = del(q)"
          , "qc = q + 0*q_t"
          , "(qc',qp',qh') = update(qc,q_t,q_tt)"
          , qc' =@ "qc'"
          , qp' =@ "qp'"
          , qh' =@ "qh'"
          ]
    flb = [ paren qc' <=> paren qc ++ " + 0*smooth " ++ paren qc
          , paren qp' <=> "smooth " ++ paren qp
          , paren qh' <=> "smooth " ++ paren qh
          ]
    sb = [ "qp" @= qp
         , "qh" @= qh
         , "(q_t,q_tt) = del(qp)"
         , "qc = qh + dt*q_t/2 - dt*dt*q_tt/12"
         , "(qc',qp',qh') = update(qc,q_t,q_tt)"
         , qc' =@ "qc'"
         , qp' =@ "qp'"
         , qh' =@ "qh'"
         ]
