

// swift-tools-version: 5.7
import PackageDescription
let CitronLexer = Target.Dependency.product(name: "CitronLexerModule", package: "citron")
let Lotsawa = Target.Dependency.product(name: "Lotsawa", package: "Lotsawa")
let package = Package(
	name: "BNFCProgramParser",
	platforms: [.macOS(.v10_15)],
	products: [.library(name: "BNFCProgramParser", targets: ["BNFCProgramParser"])],
	dependencies: [
		.package(url: "https://github.com/roop/citron.git", branch: "master"),
		.package(url: "https://github.com/hylo-lang/Lotsawa.git", branch: "main")],
	targets: [
		.target(
			name: "BNFCProgramParser",
			dependencies: [Lotsawa, CitronLexer],
			path: ".")]
)
