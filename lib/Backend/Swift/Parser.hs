module Parser where

import BNFC.CF
import BNFC.Options hiding (Backend)
import Control.Monad.State.Lazy 
import Control.Monad.Reader 
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)

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

data LotsawaGrammar = Grammar {
  rules :: [LotsawaRule],
  nontermMap  :: Map Cat Int,
  terminalMap :: Map Literal Int
}
-- | The heart of this backend: turn BNFC's rule into a Lotsawa-friendly
-- rule made of ints only. Read from the maps given in the local environment
-- to do this!
lotsawaRule :: Rule -> Reader (Map Cat Int, Map Literal Int) LotsawaRule
lotsawaRule = undefined

-- | Count how many categories there are in a set
-- of categories EXCLUDING 'TokenCat' categories.
-- Those are for the Lexer.
-- We count in a state monad so we don't have to worry about
-- where we're starting from.
categoryCount :: Set Cat -> State Int (Map Cat Int)
categoryCount = undefined

-- | Count the terminals and map them to their numbers.
-- We count in a state monad so we don't have to worry about
-- where we're starting from.
terminalCount :: [Literal] -> State Int (Map Literal Int)
terminalCount = undefined
