{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import BNFC.Backend.Base
import BNFC.CF
import BNFC.GetCF
import BNFC.Options
import Backend.Swift
import Control.Monad (zipWithM, zipWithM_)
import Data.Either (rights)
import Data.List (nub)
import Data.Set (fromList)
import System.Environment (getArgs)
import System.FilePath.Glob
import Test.QuickCheck

main :: IO ()
main = do
  paths <- pure (++) <*> defaultFiles <*> getArgs
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

defaultFiles :: IO [FilePath]
defaultFiles = glob "grammars/*.cf"

-- * QuickCheck machinery

defaultLiterals = ["Ident", "Char", "Double", "Integer", "String"]

reallyAllLits :: [Rule] -> [String]
reallyAllLits = nub . concatMap (\(Rule {rhsRule}) -> rights rhsRule)

-- | Wrapper for Arbitrary on position types, generating instances
--   by just ignoring the position.
instance (Arbitrary a) => Arbitrary (WithPosition a) where
  arbitrary = do
    thing <- arbitrary
    return $ WithPosition {wpPosition = NoPosition, wpThing = thing}

instance Arbitrary Cat where
  arbitrary =
    oneof
      [ Cat <$> arbitrary,
        TokenCat <$> arbitrary,
        ListCat <$> arbitrary,
        pure CoercCat <*> arbitrary <*> arbitrary
      ]

instance Arbitrary Rule where
  arbitrary = do
    funRule <- (arbitrary :: Gen RFun)
    valRCat <- (arbitrary :: Gen RCat)
    rhsRule <- (arbitrary :: Gen SentForm)
    return $ Rule {funRule, valRCat, rhsRule, internal = Parsable}

instance Arbitrary CF where
  arbitrary = do
    cfgRules <- (arbitrary :: Gen [Rule])
    let cfg = CFG {cfgRules}
    let cfgUsedCats = fromList $ reallyAllCats cfg
    return $ CFG {cfgRules, cfgUsedCats}
