import XCTest
import SwiftTreeSitter
import TreeSitterRemora

final class TreeSitterRemoraTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_remora())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Remora grammar")
    }
}
