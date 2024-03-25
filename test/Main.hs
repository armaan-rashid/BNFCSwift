{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import BNFC.Backend.Base
import BNFC.CF
import BNFC.GetCF
import BNFC.Options
import Backend.Swift
import Backend.Swift.Parser hiding (Rule)
import qualified Backend.Swift.Parser as P
import Control.Monad (zipWithM, zipWithM_)
import Data.Either (rights)
import Data.List (nub)
import Data.Map (Map, empty)
import qualified Data.Map as M
import Data.Set (fromList)
import Data.Tuple (swap)
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
  quickCheck prop_totalCounting
  quickCheck prop_ruleMapping
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

reallyAllTerms :: [Rule] -> [String]
reallyAllTerms = nub . concatMap (\(Rule {rhsRule}) -> rights rhsRule)

instance Show Rule where
  show r = show (wpThing (valRCat r), rhsRule r)

instance Show CF where
  show (CFG {cfgRules}) = show cfgRules

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
    let cfgKeywords = reallyAllTerms cfgRules
    return $
      CFG
        { cfgRules,
          cfgUsedCats,
          cfgKeywords,
          cfgLiterals = defaultLiterals,
          -- We ignore most of the fields, since our backend does also.
          cfgSymbols = [],
          cfgPragmas = [],
          cfgReversibleCats = [],
          cfgSignature = empty
        }

-- | Makes sure all rules in the 'LotsawaGrammar's
--   that are generated all have symbols that actually refer
--   to categories, i.e. have no -1 symbols.
prop_totalCounting :: CF -> Bool
prop_totalCounting (cfg@CFG {cfgKeywords, cfgSymbols}) =
  let (grammar@Grammar {rules}, _, _) = makeSwiftGrammar cfg
   in all (== True) (map ((all (/= -1)) . toList) rules)

-- | Makes sure the rules of the CFG are consistent with
--   the rules of the LotsawaGrammar.
prop_ruleMapping :: CF -> Bool
prop_ruleMapping (cfg@CFG {cfgRules}) =
  let (grammar@Grammar {rules, categories, terminals}, _, _) = makeSwiftGrammar cfg
   in intToCat rules categories terminals == map (\r -> (wpThing (valRCat r), rhsRule r)) cfgRules

intToCat :: [LotsawaRule] -> Map Cat Int -> Map Literal Int -> [(Cat, SentForm)]
intToCat rules cats lits = map reconstructRule rules
  where
    catInts = map swap $ M.toList cats
    litInts = map swap $ M.toList lits
    reconstructRule (P.Rule lhs rhs) = (forceLookup lhs catInts, map reconstructSent rhs)
    reconstructSent i = maybe (Left $ forceLookup i catInts) Right (lookup i litInts)
    forceLookup lhs catInts = maybe (Cat "") id (lookup lhs catInts)
