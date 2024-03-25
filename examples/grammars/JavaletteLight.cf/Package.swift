

// swift-tools-version: 5.7
import PackageDescription
let CitronLexer = Target.Dependency.product(name: "CitronLexerModule", package: "citron")
let Lotsawa = Target.Dependency.product(name: "Lotsawa", package: "Lotsawa")
let package = Package(
	name: "BNFCProgParser",
	platforms: [.macOS(.v10_15)],
	products: [.library(name: "BNFCProgParser", targets: ["BNFCProgParser"])],
	dependencies: [
		.package(url: "https://github.com/roop/citron.git", branch: "master"),
		.package(url: "https://github.com/hylo-lang/Lotsawa.git", branch: "main")],
	targets: [
		.target(
			name: "BNFCProgParser",
			dependencies: [Lotsawa, CitronLexer],
			path: ".")]
)
