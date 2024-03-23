{-# LANGUAGE LambdaCase #-}

module Backend.Swift.Lexer where

-- \^ Handles conversion of rules from BNFC's frontend into a form suitable for
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
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- | Turn the Haskell Reg type into a string suitable for Citron.
regToPattern :: Reg -> String
regToPattern REps = ""
regToPattern (RChar c) = [c]
regToPattern RDigit = "[0-9]"
regToPattern RLetter = "[A-Za-z]"
regToPattern RUpper = "[A-Z]"
regToPattern RLower = "[a-z]"
regToPattern RAny = "."
regToPattern (RStar r) = regToPattern r ++ "*"
regToPattern (RPlus r) = regToPattern r ++ "+"
regToPattern (ROpt r) = regToPattern r ++ "?"
regToPattern (RAlt r1 r2) = regToPattern r1 ++ "|" ++ regToPattern r2
regToPattern (RMinus r1 r2) = regToPattern r1 ++ "-" ++ regToPattern r2
regToPattern (RSeq r1 r2) = regToPattern r1 ++ regToPattern r2
regToPattern (RAlts s) = "[" ++ s ++ "]"
regToPattern (RSeqs s) = s

-- | Turn a token category into an actual piece of Swift code. Read from a local
--   environment of rules generating those categories.
--
--   The assumption is that the reader environment for this function does in fact
--   contain the TokenCat we're looking for which is why this implementation
--   deliberately uses partial functions: we want to raise an exception if it's not
--   because it means there's a bug in either some of the extracting code in this
--   backend or, more importantly, in BNFC's parser frontend.
lexRule :: TokenCat -> Reader (Map Cat Int, Map TokenCat Reg) String
lexRule cat = do
  (catMap, tokenMap) <- ask
  let i =
        M.lookup (TokenCat cat) catMap
          & \case
            Just n -> n
            Nothing -> -1
  return $
    M.lookup cat tokenMap
      & \case
        Just r ->
          "CitronLexer.LexingRule.regexPattern(\""
            ++ regToPattern r
            ++ "\", {_ in "
            ++ show i
            ++ "})"
        Nothing -> ""

-- | Output the Swift code that creates the Lexer in Citron from the CF grammar.
citronLexer :: Reader (Map Cat Int, Map TokenCat Reg) String
citronLexer = do
  cats <- M.keys . snd <$> ask
  rules <- mapM lexRule cats
  return $
    unlines
      ( "let lexer: CitronLexer<Int16> = CitronLexer(rules: ["
          : (mapIndent rules)
      )
      ++ "])"

mapIndent :: [String] -> [String]
mapIndent = map ("\t" ++)
