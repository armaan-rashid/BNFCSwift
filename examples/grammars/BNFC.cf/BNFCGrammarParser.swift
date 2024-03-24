// Generated by BNFC 2.9.6

import Lotsawa
import CitronLexerModule
import Foundation


struct Parser {
	let lexer: CitronLexer<Int16> = CitronLexer(rules: [
		CitronLexer.LexingRule.regexPattern("[A-Za-z]_|[0-9]|[A-Za-z]*", {_ in 28})
	])


	var grammar: DefaultGrammar = Grammar(recognizing: Symbol(id: 0))
	
	init() {
		grammar.addRule(lhs: Symbol(id: 0), rhs: [Symbol(id: 1)])
		grammar.addRule(lhs: Symbol(id: 1), rhs: [])
		grammar.addRule(lhs: Symbol(id: 1), rhs: [Symbol(id: 2)])
		grammar.addRule(lhs: Symbol(id: 1), rhs: [Symbol(id: 2), Symbol(id: 50), Symbol(id: 1)])
		grammar.addRule(lhs: Symbol(id: 1), rhs: [Symbol(id: 50), Symbol(id: 1)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 7), Symbol(id: 51), Symbol(id: 5), Symbol(id: 52), Symbol(id: 4)])
		grammar.addRule(lhs: Symbol(id: 3), rhs: [Symbol(id: 27)])
		grammar.addRule(lhs: Symbol(id: 3), rhs: [Symbol(id: 5)])
		grammar.addRule(lhs: Symbol(id: 4), rhs: [])
		grammar.addRule(lhs: Symbol(id: 4), rhs: [Symbol(id: 3), Symbol(id: 4)])
		grammar.addRule(lhs: Symbol(id: 5), rhs: [Symbol(id: 53), Symbol(id: 5), Symbol(id: 54)])
		grammar.addRule(lhs: Symbol(id: 5), rhs: [Symbol(id: 28)])
		grammar.addRule(lhs: Symbol(id: 6), rhs: [])
		grammar.addRule(lhs: Symbol(id: 6), rhs: [Symbol(id: 5)])
		grammar.addRule(lhs: Symbol(id: 6), rhs: [Symbol(id: 5), Symbol(id: 55), Symbol(id: 6)])
		grammar.addRule(lhs: Symbol(id: 7), rhs: [Symbol(id: 28)])
		grammar.addRule(lhs: Symbol(id: 7), rhs: [Symbol(id: 56)])
		grammar.addRule(lhs: Symbol(id: 7), rhs: [Symbol(id: 53), Symbol(id: 54)])
		grammar.addRule(lhs: Symbol(id: 7), rhs: [Symbol(id: 57), Symbol(id: 58), Symbol(id: 59)])
		grammar.addRule(lhs: Symbol(id: 7), rhs: [Symbol(id: 57), Symbol(id: 58), Symbol(id: 53), Symbol(id: 54), Symbol(id: 59)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 29), Symbol(id: 27)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 29), Symbol(id: 27), Symbol(id: 27)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 30), Symbol(id: 7), Symbol(id: 51), Symbol(id: 5), Symbol(id: 52), Symbol(id: 4)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 31), Symbol(id: 28), Symbol(id: 20)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 32), Symbol(id: 31), Symbol(id: 28), Symbol(id: 20)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 33), Symbol(id: 6)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 34), Symbol(id: 19), Symbol(id: 5), Symbol(id: 27)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 35), Symbol(id: 19), Symbol(id: 5), Symbol(id: 27)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 36), Symbol(id: 5), Symbol(id: 27), Symbol(id: 27), Symbol(id: 10), Symbol(id: 19)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 37), Symbol(id: 28), Symbol(id: 26)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 38), Symbol(id: 28), Symbol(id: 52), Symbol(id: 18)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 39), Symbol(id: 28), Symbol(id: 9), Symbol(id: 60), Symbol(id: 12)])
		grammar.addRule(lhs: Symbol(id: 8), rhs: [Symbol(id: 28)])
		grammar.addRule(lhs: Symbol(id: 9), rhs: [])
		grammar.addRule(lhs: Symbol(id: 9), rhs: [Symbol(id: 8), Symbol(id: 9)])
		grammar.addRule(lhs: Symbol(id: 10), rhs: [])
		grammar.addRule(lhs: Symbol(id: 10), rhs: [Symbol(id: 35), Symbol(id: 27)])
		grammar.addRule(lhs: Symbol(id: 10), rhs: [Symbol(id: 34), Symbol(id: 27)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 40), Symbol(id: 11)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 40), Symbol(id: 41), Symbol(id: 11)])
		grammar.addRule(lhs: Symbol(id: 2), rhs: [Symbol(id: 40), Symbol(id: 42)])
		grammar.addRule(lhs: Symbol(id: 11), rhs: [Symbol(id: 27)])
		grammar.addRule(lhs: Symbol(id: 11), rhs: [Symbol(id: 27), Symbol(id: 55), Symbol(id: 11)])
		grammar.addRule(lhs: Symbol(id: 12), rhs: [Symbol(id: 13), Symbol(id: 58), Symbol(id: 12)])
		grammar.addRule(lhs: Symbol(id: 13), rhs: [Symbol(id: 28), Symbol(id: 16)])
		grammar.addRule(lhs: Symbol(id: 14), rhs: [Symbol(id: 28)])
		grammar.addRule(lhs: Symbol(id: 14), rhs: [Symbol(id: 26)])
		grammar.addRule(lhs: Symbol(id: 14), rhs: [Symbol(id: 24)])
		grammar.addRule(lhs: Symbol(id: 14), rhs: [Symbol(id: 27)])
		grammar.addRule(lhs: Symbol(id: 14), rhs: [Symbol(id: 25)])
		grammar.addRule(lhs: Symbol(id: 14), rhs: [Symbol(id: 53), Symbol(id: 15), Symbol(id: 54)])
		grammar.addRule(lhs: Symbol(id: 12), rhs: [Symbol(id: 13)])
		grammar.addRule(lhs: Symbol(id: 13), rhs: [Symbol(id: 14)])
		grammar.addRule(lhs: Symbol(id: 14), rhs: [Symbol(id: 57), Symbol(id: 12), Symbol(id: 59)])
		grammar.addRule(lhs: Symbol(id: 15), rhs: [])
		grammar.addRule(lhs: Symbol(id: 15), rhs: [Symbol(id: 12)])
		grammar.addRule(lhs: Symbol(id: 15), rhs: [Symbol(id: 12), Symbol(id: 55), Symbol(id: 15)])
		grammar.addRule(lhs: Symbol(id: 16), rhs: [Symbol(id: 14)])
		grammar.addRule(lhs: Symbol(id: 16), rhs: [Symbol(id: 14), Symbol(id: 16)])
		grammar.addRule(lhs: Symbol(id: 17), rhs: [Symbol(id: 4)])
		grammar.addRule(lhs: Symbol(id: 18), rhs: [Symbol(id: 17)])
		grammar.addRule(lhs: Symbol(id: 18), rhs: [Symbol(id: 17), Symbol(id: 61), Symbol(id: 18)])
		grammar.addRule(lhs: Symbol(id: 19), rhs: [Symbol(id: 43)])
		grammar.addRule(lhs: Symbol(id: 19), rhs: [])
		grammar.addRule(lhs: Symbol(id: 20), rhs: [Symbol(id: 20), Symbol(id: 61), Symbol(id: 21)])
		grammar.addRule(lhs: Symbol(id: 21), rhs: [Symbol(id: 21), Symbol(id: 62), Symbol(id: 22)])
		grammar.addRule(lhs: Symbol(id: 22), rhs: [Symbol(id: 22), Symbol(id: 23)])
		grammar.addRule(lhs: Symbol(id: 23), rhs: [Symbol(id: 23), Symbol(id: 63)])
		grammar.addRule(lhs: Symbol(id: 23), rhs: [Symbol(id: 23), Symbol(id: 64)])
		grammar.addRule(lhs: Symbol(id: 23), rhs: [Symbol(id: 23), Symbol(id: 65)])
		grammar.addRule(lhs: Symbol(id: 23), rhs: [Symbol(id: 44)])
		grammar.addRule(lhs: Symbol(id: 23), rhs: [Symbol(id: 24)])
		grammar.addRule(lhs: Symbol(id: 23), rhs: [Symbol(id: 53), Symbol(id: 27), Symbol(id: 54)])
		grammar.addRule(lhs: Symbol(id: 23), rhs: [Symbol(id: 66), Symbol(id: 27), Symbol(id: 67)])
		grammar.addRule(lhs: Symbol(id: 23), rhs: [Symbol(id: 45)])
		grammar.addRule(lhs: Symbol(id: 23), rhs: [Symbol(id: 46)])
		grammar.addRule(lhs: Symbol(id: 23), rhs: [Symbol(id: 47)])
		grammar.addRule(lhs: Symbol(id: 23), rhs: [Symbol(id: 48)])
		grammar.addRule(lhs: Symbol(id: 23), rhs: [Symbol(id: 49)])
		grammar.addRule(lhs: Symbol(id: 20), rhs: [Symbol(id: 21)])
		grammar.addRule(lhs: Symbol(id: 21), rhs: [Symbol(id: 22)])
		grammar.addRule(lhs: Symbol(id: 22), rhs: [Symbol(id: 23)])
		grammar.addRule(lhs: Symbol(id: 23), rhs: [Symbol(id: 57), Symbol(id: 20), Symbol(id: 59)])
	
	}
	
	
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
		var recognizer: Recognizer<Int16> = Recognizer(PreprocessedGrammar(grammar))
		var tokens: [Int16] = []
		try lexer.tokenize(input, onFound: {(token) in tokens.append(token)})
		for (i,s) in tokens.enumerated() {
			recognizer.discover(Symbol(id: s), startingAt: UInt32(i))
		}
		return recognizer.forest
	}

}