{-# LANGUAGE LambdaCase #-}

module Parser (terminalCount,
               categoryCount,
               lotsawaRule,
               findEntryPoint,
               LotsawaRule (..),
               LotsawaGrammar (..)) where

import BNFC.CF
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

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
    show (Parser.Rule i ints) = "(lhs: "
                            ++ show i
                            ++ ", rhs: "
                            ++ show ints
                            ++ ")"

data LotsawaGrammar = Grammar {
  rules :: [LotsawaRule],
  recognizing :: Int
}
-- | The heart of this backend: turn BNFC's rule into a Lotsawa-friendly
-- rule made of ints only. Read from the maps given in the local environment
-- to do this!
lotsawaRule :: Rule -> Reader (Map Cat Int, Map Literal Int) LotsawaRule
lotsawaRule (BNFC.CF.Rule {valRCat = (WithPosition {wpThing = cat}), rhsRule = rule}) = do
   (catMap, litMap) <- ask
   let lhs = forceLookup $ M.lookup cat catMap
   let rhs = map (forceLookup . either (`M.lookup` catMap) (`M.lookup` litMap)) rule
   return $ Parser.Rule lhs rhs
   where forceLookup = maybe (-1) id

-- | Count how many categories there are in a set
-- of categories EXCLUDING 'TokenCat' categories.
-- Retain those excluded tokenCats in a separate set.
-- We count in a state monad so we don't have to worry about
-- where we're starting from.
categoryCount :: Set Cat -> State Int (Map Cat Int, Set TokenCat)
categoryCount cats = do
    let (tokens, nonTokens) = S.partition tokenSplit cats
    catMap <- keyCount $ S.toList nonTokens
    return (catMap, S.map tokenTake tokens)
    where tokenSplit = \case TokenCat _ -> True ; _ -> False
          tokenTake  = \case TokenCat t -> t    -- Deliberately partial.

-- | Count the terminals in the grammar.
terminalCount :: [Literal] -> State Int (Map Literal Int)
terminalCount = keyCount

-- | Count list of objects and map them to some indices.
-- We count in a state monad so we don't have to worry about
-- where we're starting from.
keyCount :: Ord a => [a] -> State Int (Map a Int)
keyCount keys = foldM (litInsert) M.empty keys
    where litInsert m key = do i <- get
                               modify (+1)
                               return $ M.insert key i m


-- | BNFC usually generates multiple parsers given an LBNF
--   grammar. For the purposes of this project we restrict the
--   LBNF grammars accepted by the Swift backend such that they
--   must include exactly one entrypoints pragma, with exactly
--   one category. This category is used as the recognizer category
--   for the Lotsawa Grammar which can only recognize one category
--   per grammar.
--
--   If excess entrypoints are given only the first one is considered
--   again only in the context of this project.
findEntryPoint :: [Pragma] -> Maybe Cat
findEntryPoint [] = Nothing
findEntryPoint (EntryPoints ((WithPosition {wpThing = cat}):_):_) = Just cat
findEntryPoint (_:xs) = findEntryPoint xs
