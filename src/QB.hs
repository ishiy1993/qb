module QB where

import qualified Data.ByteString.Builder as B
import System.IO

import QB.Types
import QB.Seed (Seed)
import QB.Scheme

generateCode :: Seed -> Code
generateCode = format . generateCodeStructure

writeCode :: FilePath -> Code -> IO ()
writeCode fn code = withFile fn WriteMode $ \h -> do
  hSetBinaryMode h True
  hSetBuffering h (BlockBuffering Nothing)
  B.hPutBuilder h code
