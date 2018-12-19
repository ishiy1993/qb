module Main where

import QB
import QB.Seed

main :: IO ()
main = do
  let fn = "tmp.fmr"
  let seed = Seed { scheme = "sl4th3"
                  , axes = ["x","y"]
                  , bases = ["r","u","v","p"]
                  , elemType = "double"
                  , eomRank = 2
                  , withFilter = False
                  }
  writeCode fn . generateCode $ seed
