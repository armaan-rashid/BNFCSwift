

// swift-tools-version: 5.7
import PackageDescription
let CitronLexer = Target.Dependency.product(name: "CitronLexerModule", package: "citron")
let Lotsawa = Target.Dependency.product(name: "Lotsawa", package: "Lotsawa")
let package = Package(
	name: "BNFCGrammarParser",
	products: [.library(name: "BNFCGrammarParser", targets: ["BNFCGrammarParser"])],
	dependencies: [
		.package(url: "https://github.com/roop/citron.git", branch: "master"),
		.package(url: "https://github.com/hylo-lang/Lotsawa.git", branch: "main")],
	targets: [
		.target(
			name: "BNFCGrammarParser",
			dependencies: [Lotsawa, CitronLexer],
			path: ".")]
)
