module Main where

import QB
import QB.Seed

import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      seed <- getSeed fn
      let output = (scheme seed) ++ ".fmr"
      writeCode output . generateCode $ seed
    _ -> die "Need a yaml file"
