{-# LANGUAGE RecordWildCards #-}
module Swift (makeSwift) where

import BNFC.Backend.Base
import BNFC.CF
import BNFC.Options hiding (Backend)
import Control.Monad.State.Lazy 
import Control.Monad.Reader 
import Backend.Swift.Lexer
import Backend.Swift.Parser 
import Data.Set (Set)

-- | Run the context free grammar produced by BNFC frontend
-- through a conversion process that turns it into a proper
-- Lotsawa grammar in Swift code.

-- Right now there's no meaningful options for the Swift backend
-- so we just ignore them, but they're there to satisfy the type 
-- signature for all the other backends.
makeSwift :: SharedOptions -> CF -> Backend 
makeSwift opts (CFG {cfgUsedCats = cats, 
                     cfgLiterals = lits, 
                     cfgRules    = rules, ..}) = do
  -- Count everything!
  let (nontermMap, i) = runState (categoryCount cats) 0
  let termMap = evalState (terminalCount lits) i
  -- Gather the lexing rules.
  -- Convert raw rules into Lotsawa friendly ones.
  let lotsawaRules = runReader (mapM lotsawaRule rules) (nontermMap, termMap)
  -- Write this all into Swift.
  -- Stitch together the lexing and parsing code somehow.
  return ()

