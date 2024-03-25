

// swift-tools-version: 5.7
import PackageDescription
let CitronLexer = Target.Dependency.product(name: "CitronLexerModule", package: "citron")
let Lotsawa = Target.Dependency.product(name: "Lotsawa", package: "Lotsawa")
let package = Package(
	name: "BNFCModuleParser",
	platforms: [.macOS(.v10_15)],
	products: [.library(name: "BNFCModuleParser", targets: ["BNFCModuleParser"])],
	dependencies: [
		.package(url: "https://github.com/roop/citron.git", branch: "master"),
		.package(url: "https://github.com/hylo-lang/Lotsawa.git", branch: "main")],
	targets: [
		.target(
			name: "BNFCModuleParser",
			dependencies: [Lotsawa, CitronLexer],
			path: ".")]
)
