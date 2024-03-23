module Main where

import BNFC.Backend.Base
import BNFC.CF (CF)
import BNFC.GetCF
import BNFC.Options
import Backend.Swift
import Control.Monad (zipWithM, zipWithM_)
import System.Environment (getArgs)

main :: IO ()
main = do
  paths <- (defaultFiles ++) <$> getArgs
  sources <- mapM (readFile) paths
  let options = map createOptions paths
  cfgs <- zipWithM runFrontEnd options sources
  zipWithM_ writeTestFiles options cfgs
  where
    writeTestFiles = \opt -> \cfg ->
      writeFiles (outDir opt) (makeSwift opt cfg)

-- | Given filename, run BNFC's frontend on it and get the CF.
runFrontEnd :: SharedOptions -> String -> IO CF
runFrontEnd = (flip parseCF) TargetHaskell

createOptions :: FilePath -> SharedOptions
createOptions path =
  defaultOptions
    { lbnfFile = path,
      lang = "swift",
      outDir = "examples/" ++ path
    }

defaultFiles :: [FilePath]
defaultFiles = ["grammars/BNFC.cf"]
