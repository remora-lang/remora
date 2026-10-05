package tree_sitter_remora_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_remora "github.com/remora-lang/remora/tree/main/utils/tree-sitter-remora/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_remora.Language())
	if language == nil {
		t.Errorf("Error loading Remora grammar")
	}
}
