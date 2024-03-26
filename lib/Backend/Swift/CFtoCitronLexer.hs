{-# LANGUAGE LambdaCase #-}

module Backend.Swift.CFtoCitronLexer where

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
import BNFC.CF hiding (comments)
import Control.Monad.Reader (MonadReader (ask), Reader)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- | Data needed for assembling lexer rules.
type LexData = (Map Cat Int, Map TokenCat Reg, ([(String, String)], [String]))

data CitronLexerRule
  = RegexToken Reg Int
  | LineComment String
  | MultiLineComment String String

instance Show CitronLexerRule where
  show (RegexToken r i) =
    "CitronLexer.LexingRule.regexPattern(\""
      ++ regToPattern r
      ++ "\", {_ in "
      ++ show i
      ++ "}),"
  show (LineComment s) =
    "CitronLexer.LexingRule.regexPattern(\""
      ++ escapeCommentStr s
      ++ ".*$\", {_ in nil}),"
  show (MultiLineComment s t) =
    "CitronLexer.LexingRule.regex(try! NSRegularExpression(pattern: \""
      ++ escapeCommentStr s
      ++ ".*?"
      ++ escapeCommentStr t
      ++ "\", options: NSRegularExpression.Options.dotMatchesLineSeparators), {_ in nil}),"

-- | Turn a token category into an actual piece of Swift code. Read from a local
--   environment of rules generating those categories.
lexRule :: TokenCat -> Reader LexData CitronLexerRule
lexRule cat =
  -- \^  Token category to convert
  do
    (catMap, tokenMap, _) <- ask
    let i = maybe (-1) id $ M.lookup (TokenCat cat) catMap
    let r = maybe REps id $ M.lookup cat tokenMap
    return $ RegexToken r i

-- | Output the Swift code that creates the Lexer in Citron from the CF grammar.
citronLexer :: Reader LexData String
citronLexer = do
  (_, tokenCats, (mComments, sComments)) <- ask
  rules <- mapM lexRule (M.keys tokenCats)
  let mCommentRules = map (\(s, t) -> MultiLineComment s t) mComments
  let sCommentRules = map LineComment sComments
  return $
    unlines
      ( "let lexer: CitronLexer<Int16> = CitronLexer(rules: ["
          : (mapIndent (map show $ rules ++ sCommentRules ++ mCommentRules))
      )
      ++ "])"

-- * Long boring string processing utilities.

-- | Indents every string in the list.
mapIndent :: [String] -> [String]
mapIndent = map ("\t" ++)

-- | Comment delimiters may have characters in them that need to be escaped.
-- This function does just that according to CitronLexer's specification to
-- generate regular expressions.
escapeCommentStr :: String -> String
escapeCommentStr = concatMap escape
  where
    escape '\\' = "\\\\"
    escape '?' = "\\?"
    escape '+' = "\\+"
    escape '^' = "\\^"
    escape '|' = "\\|"
    escape '[' = "\\["
    escape ']' = "\\]"
    escape '(' = "\\("
    escape ')' = "\\)"
    escape c = [c]

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
