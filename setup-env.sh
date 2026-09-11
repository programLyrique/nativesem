#!/bin/bash
# Setup environment variables for r-c-typing project
# This script sets up paths to the tree-sitter installation from r-parser
#
# Usage: source setup-env.sh
#        R_PARSER_PATH=/path/to/r-parser source setup-env.sh
#
# By default r-parser is expected next to this repository (../r-parser); set
# R_PARSER_PATH in the environment to point somewhere else.

# ============================================================================
# CONFIGURATION - r-parser LOCATION
# ============================================================================
# Directory holding this script, so the default works whatever the caller's cwd.
SETUP_ENV_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
R_PARSER_PATH="${R_PARSER_PATH:-${SETUP_ENV_DIR}/../r-parser}"

# Check if r-parser exists at the configured path
if [ ! -d "$R_PARSER_PATH" ]; then
    echo "ERROR: r-parser directory not found at: $R_PARSER_PATH"
    echo "Set R_PARSER_PATH to your r-parser installation, or clone it next to"
    echo "this repository (as ${SETUP_ENV_DIR}/../r-parser)"
    echo "You can get r-parser from: https://github.com/E-Sh4rk/r-parser"
    return 1 2>/dev/null || exit 1
fi

# Resolve to an absolute, symlink-free path (the checks above guarantee it exists).
R_PARSER_PATH="$(cd "$R_PARSER_PATH" && pwd)"

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
