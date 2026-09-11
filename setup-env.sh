#!/bin/bash
# Setup environment variables for r-c-typing project
# This script sets up paths to the tree-sitter installation from r-parser
#
# Usage: source setup-env.sh
#
# You must modify R_PARSER_PATH to point to your r-parser installation

# ============================================================================
# CONFIGURATION - UPDATE THIS PATH TO YOUR r-parser INSTALLATION
# ============================================================================
R_PARSER_PATH="/home/pierre/Documents/RLanguage/types-for-R/r-parser"  # <-- UPDATE THIS PATH

# Check if r-parser exists at the configured path
if [ ! -d "$R_PARSER_PATH" ]; then
    echo "ERROR: r-parser directory not found at: $R_PARSER_PATH"
    echo "Please update R_PARSER_PATH in setup-env.sh to point to your r-parser installation"
    echo "You can get r-parser from: https://github.com/E-Sh4rk/r-parser"
    return 1 2>/dev/null || exit 1
fi

# ============================================================================
# ENVIRONMENT SETUP
# ============================================================================
TREE_SITTER_LIB="${R_PARSER_PATH}/core/tree-sitter/lib"
TREE_SITTER_INCLUDE="${R_PARSER_PATH}/core/tree-sitter/include"

# Verify tree-sitter directories exist
if [ ! -d "$TREE_SITTER_LIB" ]; then
    echo "ERROR: tree-sitter lib directory not found at: $TREE_SITTER_LIB"
    echo "Make sure r-parser is properly compiled"
    return 1 2>/dev/null || exit 1
fi

if [ ! -d "$TREE_SITTER_INCLUDE" ]; then
    echo "ERROR: tree-sitter include directory not found at: $TREE_SITTER_INCLUDE"
    echo "Make sure r-parser is properly compiled"
    return 1 2>/dev/null || exit 1
fi

# Set environment variables
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+${LD_LIBRARY_PATH}:}${TREE_SITTER_LIB}"
export TREESITTER_INCDIR="${TREE_SITTER_INCLUDE}"
export TREESITTER_LIBDIR="${TREE_SITTER_LIB}"

echo "✓ Environment configured for r-c-typing"
echo "  R_PARSER_PATH:      $R_PARSER_PATH"
echo "  TREESITTER_INCDIR:  $TREESITTER_INCDIR"
echo "  TREESITTER_LIBDIR:  $TREESITTER_LIBDIR"
echo "  LD_LIBRARY_PATH:    $LD_LIBRARY_PATH"
