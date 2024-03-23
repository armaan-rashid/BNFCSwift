

// swift-tools-version: 5.7
let CitronLexer = Target.Dependency.product(name: "CitronLexerModule", package: "citron")
let Lotsawa = Target.Dependency.product(name: "Lotsawa", package: "Lotsawa")
let package = Package(
	name: "BNFCGrammarParser",
	platforms: [.macOS(.v10_15)],
	dependencies: [
		.package(url: "https://github.com/roop/citron.git", branch: "master"),
		.package(url: "https://github.com/hylo-lang/Lotsawa.git", branch: "main")],
	targets: [
		.target(
			name: "GrammarParser",
			dependencies: [Lotsawa, "BNFCGrammarParser"],
			path: ".")]
)
