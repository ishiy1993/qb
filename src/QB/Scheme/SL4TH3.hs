{-# LANGUAGE OverloadedStrings #-}
module QB.Scheme.SL4TH3 (withSL4TH3) where

import qualified Data.ByteString.Builder as B
import Data.List (nub, sort, group, findIndex)
import Data.Maybe (fromJust, fromMaybe)

import QB.Types
import QB.Seed (Seed)
import qualified QB.Seed as S

-- もうちょっと数値の扱いを改善したい
-- +1 -> +
-- 0 -> ""
-- など
mkDiff :: (Int,Int,String,Int) -> B.Builder
mkDiff (dim,i,a,o)
  | o == 1 = build [(-2,1),(-1,-8),(1,8),(2,-1)] ("12*d" <> a')
  | o == 2 = build [(-2,-1),(-1,16),(0,-30),(1,16),(2,-1)] ("12*d" <> a' <> "**2")
  | o == 3 = build [(-2,-1),(-1,2),(1,-2),(2,1)] ("2*d" <> a' <> "**3")
  | o == 4 = build [(-2,1),(-1,-4),(0,6),(1,-4),(2,1)] ("d" <> a' <> "**4")
  | otherwise = error "Error in mkDiff"
  where
    a' = B.stringUtf8 a
    build tbl c0 = "(" <> sepWith " " [renderInt c <> mkIdx (dim,i) s | (s,c) <- tbl] <> ")/(" <> c0 <> ")"
    renderInt x | x == -1 = "-"
                | x > 0 = "+" <> B.intDec x <> "*"
                | otherwise = B.intDec x <> "*"

mkSmooth :: (Int,Int) -> B.Builder
mkSmooth (dim,i) = "11*" <> q 0 <> "/16"
                <> " + 15*(" <> q 1 <> " + " <> q (-1) <> ")/64"
                <> " - 3*(" <> q 2 <> " + " <> q (-2) <> ")/32"
                <> " + (" <> q 3 <> " + " <> q (-3) <> ")/64"
  where
    q :: Int -> B.Builder
    q = mkIdx (dim,i)

mkIdx :: (Int,Int) -> Int -> B.Builder
mkIdx (dim,i) s = "q" <> bckt ["i" <> B.intDec j <> if j == i then renderIdx s else "" | j <- [1..dim]]
  where renderIdx x | x == 0 = ""
                    | x > 0 = "+" <> B.intDec x
                    | otherwise = B.intDec x

withSL4TH3 :: Seed -> CodeStructure
withSL4TH3 s = CodeStructure { target = S.scheme s
                             , axes = as
                             , gridStruct = [q <> x | x <- ["c","p","h"], q <- qs]
                             , elemType = B.stringUtf8 $ S.elemType s
                             , defs = ds
                             , initBody = ib
                             , firstStepBody = Just fsb
                             , filterBody = if S.withFilter s then Just flb else Nothing
                             , stepBody = sb
                             }
  where
    as = map B.stringUtf8 $ S.axes s
    dim = length as
    qs = map B.stringUtf8 $ S.bases s
    [qc,qp,qh] = [map (<>x) | x <- ["c","p","h"]] <*> [qs]
    [qc',qp',qh'] = map (<>"'") <$> [qc,qp,qh]
    -- TODO: Refactoring ↓
    delIdx = tail $ concat $ map (nub . map sort) $ scanl (\acc a -> (:) <$> a <*> acc) [[]] (replicate (2*S.eomRank s) $ S.axes s)
    isPure = (==1) . length . group
    pures = filter isPure delIdx
    cross = filter (not . isPure) delIdx
    mkD i = "d_" <> B.stringUtf8 (concat i)
    diffs = sepWith "\n" $ ["#DIFF"]
                         ++ [mkD d <=> "fun(q) " <> (mkDiff (dim, 1 + fromJust (findIndex (==a) $ S.axes s),a,length d)) | d <- pures, let a = head d]
                         ++ [mkD d <=> sepWith " . " (map mkD $ group d) | d <- cross]
    smooth = sepWith "\n" $ [ "#SMOOTH" ]
                          ++ ["smooth_" <> a <=> "fun(q) " <> (mkSmooth (dim,i)) | (i,a) <- zip [1..dim] as]
                          ++ ["smooth" <=> sepWith " . " ["smooth_" <> a | a <- as]]
    updateF = defFun "update" "(qc,q_t,q_tt)" "(qc',qp',qh')"
                [ "qc' = qc"
                , "qp' = qc + dt*q_t + dt*dt*q_tt/2"
                , "qh' = qc + dt*q_t/2 + dt*dt*q_tt/12"
                ]
    delF = defFun "del" "(q)" "(q_t,q_tt)" $
            [ qs =@ "q" ]
            ++ [ (map (<>"_"<>d) qs) =@ ("d_"<> d <> " q") | d0 <- delIdx, let d = B.stringUtf8 $ concat d0 ] ++
            [ "#EOM"
            , B.stringUtf8 $ fromMaybe "" (S.eom s)
            , "q_t" @= map (<>"_t") qs
            , "q_tt" @= map (<>"_tt") qs
            ]
    ds = ["#PARAM", B.stringUtf8 $ fromMaybe "" (S.params s), "#INIT", B.stringUtf8 $ fromMaybe "" (S.initialCondition s), diffs, smooth, updateF, delF]
    ib = [q <> "c" <> bckt ["i" <> B.intDec i | i <- [1..dim]] <=> "initialize_" <> q <> paren ["i" <> B.intDec i <> "*d" <> a | (i,a) <- zip [1..dim] as] | q <- qs]
    fsb = [ "q" @= qc
          , "(q_t,q_tt) = del(q)"
          , "qc = q + 0*q_t"
          , "(qc',qp',qh') = update(qc,q_t,q_tt)"
          , qc' =@ "qc'"
          , qp' =@ "qp'"
          , qh' =@ "qh'"
          ]
    flb = [ paren qc' <=> paren qc <> " + 0*smooth " <> paren qc
          , paren qp' <=> "smooth " <> paren qp
          , paren qh' <=> "smooth " <> paren qh
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
