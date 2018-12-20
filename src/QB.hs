module QB where

import QB.Types
import QB.Seed (Seed)
import QB.Scheme

generateCode :: Seed -> Code
generateCode = format . generateCodeStructure

writeCode :: FilePath -> Code -> IO ()
writeCode = writeFile
