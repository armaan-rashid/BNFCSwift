{-# LANGUAGE NamedFieldPuns #-}

module Backend.Swift where

import BNFC.Backend.Base
  ( Backend,
    GeneratedFile (GeneratedFile, fileContent, fileName, makeComment),
  )
import BNFC.CF
  ( CF,
    CFG (..),
    Cat (TokenCat),
    comments,
    reallyAllCats,
    tokenPragmas,
  )
import BNFC.Options (SharedOptions)
import Backend.Swift.CFtoCitronLexer (citronLexer, mapIndent)
import Backend.Swift.CFtoLotsawaParser
  ( LotsawaGrammar
      ( Grammar,
        categories,
        recognizing,
        rules,
        terminals
      ),
    categoryCount,
    lotsawaRule,
    recognizingSym,
    terminalCount,
  )
import Control.Monad.Reader (runReader)
import Control.Monad.State.Lazy (runState)
import Control.Monad.Writer (MonadWriter (tell))
import qualified Data.Map.Strict as M

-- | Run the context free grammar produced by BNFC frontend
-- through a conversion process that turns it into a proper
-- Lotsawa grammar in Swift code, raw Swift code for the
-- CitronLexer, and the category the Parser recognizes.
makeSwiftGrammar :: CF -> (LotsawaGrammar, String, Cat)
makeSwiftGrammar
  ( cfg@CFG
      { cfgRules,
        cfgKeywords,
        cfgSymbols
      }
    ) =
    -- \^ Context-Free Grammar to build a parser for
    do
      -- Count everything!
      let totalCats = termsAndNonterms cfg
          (catMap, i) = count 0 (categoryCount totalCats)
          (termMap, _) = count i $ (terminalCount (cfgKeywords ++ cfgSymbols))
          tokenMap = M.fromList (tokenPragmas cfg)

          -- Gather the lexing code.
          lexCode = with (catMap, tokenMap, comments cfg) citronLexer

          -- Convert raw rules into Lotsawa friendly ones, create the grammar.
          lotsawaRules = with (catMap, termMap) (mapM lotsawaRule (cfgRules))
          (mainCat, lotsawaSymbol) = with catMap (recognizingSym cfg)
          lotsawaGrammar =
            Grammar
              { recognizing = lotsawaSymbol,
                rules = lotsawaRules,
                categories = catMap,
                terminals = termMap
              }
       in (lotsawaGrammar, lexCode, mainCat)
    where
      with = flip runReader
      count = flip runState

-- | Wrapper for 'makeSwiftGrammar' that stitches everything
--   together, currently ignoring the options.
makeSwift ::
  SharedOptions ->
  -- \^ Options for parser generation (not implemented)
  CF ->
  -- \^ Context-Free Grammar to build a parser for
  Backend
makeSwift _ cfg =
  do
    (lotsawaGrammar, lexCode, mainCat) <- return $ makeSwiftGrammar cfg
    -- Stitch together the lexing and parsing code.
    let pkgName = "BNFC" ++ show mainCat ++ "Parser"
    let mainParserFile =
          unlines $
            [ imports,
              "",
              "struct Parser {"
            ]
              ++ body
              ++ ["}"]
          where
            body = map (unlines . mapIndent . lines) [lexCode, "", show lotsawaGrammar, "", swiftParse mainCat]
    tell
      [ GeneratedFile
          { fileName = "Package.swift",
            -- No comment for package file since first line must contain the swift-tools-version
            makeComment = const "",
            fileContent = swiftPackage mainCat
          },
        GeneratedFile
          { fileName = pkgName ++ ".swift",
            makeComment = const "// Generated by BNFC 2.9.6",
            fileContent = mainParserFile
          }
      ]

-- | Since nonterminals and terminals are categories for Lotsawa gather all of them.
termsAndNonterms :: CF -> [Cat]
termsAndNonterms cfg =
  reallyAllCats cfg
    ++ map TokenCat (cfgLiterals cfg)
    ++ map (TokenCat . fst) (tokenPragmas cfg)

-- * Code constants

-- Functions that return the necessary Swift boilerplate as constant Strings

-- | Imports necessary Swift packages.
imports :: String
imports =
  unlines $
    map
      ("import " ++)
      [ "Lotsawa",
        "CitronLexerModule",
        "Foundation"
      ]

-- | Painful boilerplate for the Package.Swift file.
swiftPackage :: Cat -> String
swiftPackage cat =
  let pkgName = "BNFC" ++ show cat ++ "Parser"
   in unlines $
        [ "// swift-tools-version: 5.7",
          "import PackageDescription",
          "let CitronLexer = Target.Dependency.product(name: \"CitronLexerModule\", package: \"citron\")",
          "let Lotsawa = Target.Dependency.product(name: \"Lotsawa\", package: \"Lotsawa\")",
          "let package = Package("
        ]
          ++ mapIndent
            ( [ "name: \"" ++ pkgName ++ "\",",
                "platforms: [.macOS(.v10_15)],",
                "products: [.library(name: \"" ++ pkgName ++ "\", targets: [\"" ++ pkgName ++ "\"])],",
                "dependencies: ["
              ]
                ++ mapIndent
                  ( [ ".package(url: \"https://github.com/roop/citron.git\", branch: \"master\"),",
                      ".package(url: \"https://github.com/hylo-lang/Lotsawa.git\", branch: \"main\")],"
                    ]
                  )
                ++ ["targets: ["]
                ++ mapIndent
                  ( [ ".target("
                    ]
                      ++ mapIndent
                        ( [ "name: \"" ++ pkgName ++ "\",",
                            "dependencies: [Lotsawa, CitronLexer],",
                            "path: \".\")]"
                          ]
                        )
                  )
            )
          ++ [")"]

-- | Use a Swift function to read, lex, and parse a string!
swiftParse :: Cat -> String
swiftParse cat =
  unlines $
    [ "public func run" ++ show cat ++ "Parser(input: String) throws -> Forest<Int16> {"
    ]
      ++ mapIndent
        ( [ "var recognizer: Recognizer<Int16> = Recognizer(PreprocessedGrammar(grammar))",
            "var tokens: [Int16] = []",
            "try lexer.tokenize(input, onFound: {(token) in tokens.append(token)})",
            "for (i,s) in tokens.enumerated() {"
          ]
            ++ mapIndent
              [ "recognizer.discover(Symbol(id: s), startingAt: UInt32(i))"
              ]
            ++ [ "}",
                 "return recognizer.forest"
               ]
        )
      ++ ["}"]
