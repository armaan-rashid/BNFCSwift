// Generated by BNFC 2.9.6

import Lotsawa
import CitronLexerModule
import Foundation


let lexer: CitronLexer<Int16> = CitronLexer(rules: [
	LexingRule.regexPattern("[A-Za-z]_|[0-9]|[A-Za-z]*", {_ in 28})
])

var grammar: DefaultGrammar = Grammar(recognizing: 0)

grammar.addRule(lhs: 0, rhs: [1])
grammar.addRule(lhs: 1, rhs: [])
grammar.addRule(lhs: 1, rhs: [2])
grammar.addRule(lhs: 1, rhs: [2,50,1])
grammar.addRule(lhs: 1, rhs: [50,1])
grammar.addRule(lhs: 2, rhs: [7,51,5,52,4])
grammar.addRule(lhs: 3, rhs: [27])
grammar.addRule(lhs: 3, rhs: [5])
grammar.addRule(lhs: 4, rhs: [])
grammar.addRule(lhs: 4, rhs: [3,4])
grammar.addRule(lhs: 5, rhs: [53,5,54])
grammar.addRule(lhs: 5, rhs: [28])
grammar.addRule(lhs: 6, rhs: [])
grammar.addRule(lhs: 6, rhs: [5])
grammar.addRule(lhs: 6, rhs: [5,55,6])
grammar.addRule(lhs: 7, rhs: [28])
grammar.addRule(lhs: 7, rhs: [56])
grammar.addRule(lhs: 7, rhs: [53,54])
grammar.addRule(lhs: 7, rhs: [57,58,59])
grammar.addRule(lhs: 7, rhs: [57,58,53,54,59])
grammar.addRule(lhs: 2, rhs: [29,27])
grammar.addRule(lhs: 2, rhs: [29,27,27])
grammar.addRule(lhs: 2, rhs: [30,7,51,5,52,4])
grammar.addRule(lhs: 2, rhs: [31,28,20])
grammar.addRule(lhs: 2, rhs: [32,31,28,20])
grammar.addRule(lhs: 2, rhs: [33,6])
grammar.addRule(lhs: 2, rhs: [34,19,5,27])
grammar.addRule(lhs: 2, rhs: [35,19,5,27])
grammar.addRule(lhs: 2, rhs: [36,5,27,27,10,19])
grammar.addRule(lhs: 2, rhs: [37,28,26])
grammar.addRule(lhs: 2, rhs: [38,28,52,18])
grammar.addRule(lhs: 2, rhs: [39,28,9,60,12])
grammar.addRule(lhs: 8, rhs: [28])
grammar.addRule(lhs: 9, rhs: [])
grammar.addRule(lhs: 9, rhs: [8,9])
grammar.addRule(lhs: 10, rhs: [])
grammar.addRule(lhs: 10, rhs: [35,27])
grammar.addRule(lhs: 10, rhs: [34,27])
grammar.addRule(lhs: 2, rhs: [40,11])
grammar.addRule(lhs: 2, rhs: [40,41,11])
grammar.addRule(lhs: 2, rhs: [40,42])
grammar.addRule(lhs: 11, rhs: [27])
grammar.addRule(lhs: 11, rhs: [27,55,11])
grammar.addRule(lhs: 12, rhs: [13,58,12])
grammar.addRule(lhs: 13, rhs: [28,16])
grammar.addRule(lhs: 14, rhs: [28])
grammar.addRule(lhs: 14, rhs: [26])
grammar.addRule(lhs: 14, rhs: [24])
grammar.addRule(lhs: 14, rhs: [27])
grammar.addRule(lhs: 14, rhs: [25])
grammar.addRule(lhs: 14, rhs: [53,15,54])
grammar.addRule(lhs: 12, rhs: [13])
grammar.addRule(lhs: 13, rhs: [14])
grammar.addRule(lhs: 14, rhs: [57,12,59])
grammar.addRule(lhs: 15, rhs: [])
grammar.addRule(lhs: 15, rhs: [12])
grammar.addRule(lhs: 15, rhs: [12,55,15])
grammar.addRule(lhs: 16, rhs: [14])
grammar.addRule(lhs: 16, rhs: [14,16])
grammar.addRule(lhs: 17, rhs: [4])
grammar.addRule(lhs: 18, rhs: [17])
grammar.addRule(lhs: 18, rhs: [17,61,18])
grammar.addRule(lhs: 19, rhs: [43])
grammar.addRule(lhs: 19, rhs: [])
grammar.addRule(lhs: 20, rhs: [20,61,21])
grammar.addRule(lhs: 21, rhs: [21,62,22])
grammar.addRule(lhs: 22, rhs: [22,23])
grammar.addRule(lhs: 23, rhs: [23,63])
grammar.addRule(lhs: 23, rhs: [23,64])
grammar.addRule(lhs: 23, rhs: [23,65])
grammar.addRule(lhs: 23, rhs: [44])
grammar.addRule(lhs: 23, rhs: [24])
grammar.addRule(lhs: 23, rhs: [53,27,54])
grammar.addRule(lhs: 23, rhs: [66,27,67])
grammar.addRule(lhs: 23, rhs: [45])
grammar.addRule(lhs: 23, rhs: [46])
grammar.addRule(lhs: 23, rhs: [47])
grammar.addRule(lhs: 23, rhs: [48])
grammar.addRule(lhs: 23, rhs: [49])
grammar.addRule(lhs: 20, rhs: [21])
grammar.addRule(lhs: 21, rhs: [22])
grammar.addRule(lhs: 22, rhs: [23])
grammar.addRule(lhs: 23, rhs: [57,20,59])

let cats: [Int : String] = [
	8 : "Arg",
	5 : "Cat",
	2 : "Def",
	12 : "Exp",
	0 : "Grammar",
	3 : "Item",
	7 : "Label",
	19 : "MinimumSize",
	17 : "RHS",
	20 : "Reg",
	10 : "Separation",
	24 : "Char",
	25 : "Double",
	28 : "Identifier",
	26 : "Integer",
	27 : "String",
	9 : "[Arg]",
	6 : "[Cat]",
	1 : "[Def]",
	15 : "[Exp]",
	4 : "[Item]",
	18 : "[RHS]",
	11 : "[String]",
	16 : "[Exp2]",
	13 : "Exp1",
	14 : "Exp2",
	21 : "Reg1",
	22 : "Reg2",
	23 : "Reg3",
]

let terminals: [Int : String] = [
	57 : "(",
	59 : ")",
	63 : "*",
	64 : "+",
	55 : ",",
	62 : "-",
	51 : ".",
	58 : ":",
	52 : "::=",
	50 : ";",
	60 : "=",
	65 : "?",
	53 : "[",
	54 : "]",
	56 : "_",
	49 : "char",
	37 : "coercions",
	29 : "comment",
	39 : "define",
	36 : "delimiters",
	45 : "digit",
	33 : "entrypoints",
	44 : "eps",
	30 : "internal",
	40 : "layout",
	46 : "letter",
	48 : "lower",
	43 : "nonempty",
	32 : "position",
	38 : "rules",
	34 : "separator",
	41 : "stop",
	35 : "terminator",
	31 : "token",
	42 : "toplevel",
	47 : "upper",
	66 : "{",
	61 : "|",
	67 : "}",
]


public func runGrammarParser(input: String) throws -> Forest<Int16> {
	let recognizer: Recognizer<Int16> = Recognizer(PreprocessedGrammar(grammar))
	var tokens: [Int16] = []
	lexer.tokenize(input, onFound: {(token) in tokens.append(token)})
	for (i,s) in tokens.enumerated() {
		recognizer.discover(s, startingAt: i)
	}
	return recognizer.forest
}

