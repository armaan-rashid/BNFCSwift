{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Backend.Swift.Parser
  ( terminalCount,
    categoryCount,
    lotsawaRule,
    recognizingSym,
    LotsawaRule (..),
    LotsawaGrammar (..),
  )
where

import BNFC.CF
import Control.Monad (foldM)
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- | Type of Lotsawa grammar rules. Lotsawa grammar doesn't operate
--   with any string or similar types to populate its grammar rules:
--   everything must be provided as a fixed-width integer.
--   The sole constructor of this type takes one integer on the LHS and
--   a list of integers on the RHS corresponding to terminals / nonterminals.

--   An 8-bit integer might suffice but we default to 16 bits so there
--   can be as many as 65536 terminals and nonterminals (combined) in a
--   grammar. That said, there's no reason not to make this polymorphic to
--   any fixed-width integer in the future.
data LotsawaRule = Rule Int [Int]
  deriving (Eq)

-- | This show instance renders the rule in Swift code
--   as suitable function parameters for Lotsawa's grammar code, e.g.
--   `show (Rule 5 [2, 1])` ==
--   ```swift
--   (lhs: 5, rhs: [2, 1])
--   ```
instance Show LotsawaRule where
  show (Backend.Swift.Parser.Rule i ints) =
    "(lhs: "
      ++ show i
      ++ ", rhs: "
      ++ show ints
      ++ ")"

data LotsawaGrammar = Grammar
  { rules :: [LotsawaRule],
    recognizing :: Int
  }

-- | The actual Swift code that initializes the Lotsawa Grammar correctly
--   in Swift.
instance Show LotsawaGrammar where
  show (Grammar {rules, recognizing}) =
    unlines $
      ( "var grammar: DefaultGrammar = Grammar(recognizing: "
          ++ show recognizing
          ++ ")"
      )
        : ""
        : map (("grammar.addRule" ++) . show) rules

-- | The heart of this backend: turn BNFC's rule into a Lotsawa-friendly
-- rule made of ints only. Read from the maps given in the local environment
-- to do this!
lotsawaRule :: Rule -> Reader (Map Cat Int, Map Literal Int) LotsawaRule
lotsawaRule (BNFC.CF.Rule {valRCat = (WithPosition {wpThing = cat}), rhsRule = rule}) = do
  (catMap, litMap) <- ask
  let lhs = forceLookup $ M.lookup cat catMap
  let rhs = map (forceLookup . either (`M.lookup` catMap) (`M.lookup` litMap)) rule
  return $ Backend.Swift.Parser.Rule lhs rhs
  where
    forceLookup = maybe (-1) id

-- | Count how many categories there are in a set
-- of categories EXCLUDING 'TokenCat' categories.
-- Retain those excluded tokenCats in a separate set.
-- We count in a state monad so we don't have to worry about
-- where we're starting from.
categoryCount :: [Cat] -> State Int (Map Cat Int)
categoryCount = keyCount

-- | Count the terminals in the grammar.
terminalCount :: [Literal] -> State Int (Map Literal Int)
terminalCount = keyCount

-- | Count list of objects and map them to some indices.
-- We count in a state monad so we don't have to worry about
-- where we're starting from.
keyCount :: (Ord a) => [a] -> State Int (Map a Int)
keyCount keys = foldM (keyInsert) M.empty keys
  where
    keyInsert m key = do
      i <- get
      modify (+ 1)
      return $ M.insert key i m

-- | Get the symbol to recognize for the parser.
recognizingSym :: CF -> Reader (Map Cat Int) (Cat, Int)
recognizingSym cfg = do
  let parseCat = NE.head $ allEntryPoints cfg
  (M.lookup parseCat <$> ask) <&> \case
    Just i -> (parseCat, i)
    Nothing -> (parseCat, -1)
