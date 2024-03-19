module Lexer where
-- ^ Handles conversion of rules from BNFC's frontend into a form suitable for 
--   the CitronLexer that Lotsawa uses as a Lexing module.
--   Basically need to output Swift code in this Swift enum
--   ```swift
--   public enum LexingRule {
--      case string(String, TokenData?)
--      case regex(NSRegularExpression, (String) -> TokenData?)
--      case regexPattern(String, (String) -> TokenData?)
--   }
--   ```
--   which comes from the [CitronLexer](https://github.com/roop/citron/blob/master/Sources/CitronLexerModule/CitronLexer.swift) module.

import BNFC.Abs (Reg (..))
import BNFC.CF
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict

-- | Get the token rules out of a list of pragmas. 
tokenRules :: [Pragma] -> Map TokenCat Reg
tokenRules = undefined

-- | Turn a token category into an actual piece of Swift code. Read from a local
--   environment of rules generating those categories.
lexRule :: TokenCat -> Reader (Map TokenCat Reg) String
lexRule = undefined
