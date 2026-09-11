# R C Typing - AI Coding Agent Instructions

## Project Overview

This OCaml project analyzes and types C functions that use the R C API. It parses C code with Tree-sitter, transforms it through multiple intermediate representations (CST → PAst → Ast → MLsem), and performs type inference using the MLsem framework with RSTT (R Set-Theoretic Types).

## Architecture & Data Flow

The analysis pipeline has **four distinct AST representations**:

1. **CST** (Concrete Syntax Tree) - Raw Tree-sitter output from the C parser (`c-parser/`)
2. **PAst** (Parsed AST) - Position-annotated, untyped AST with string identifiers ([lib/PAst.ml](lib/PAst.ml))
3. **Ast** - Typed AST with `Variable.t` instead of strings, `kind VarMap.t` tracking constant/label info ([lib/ast.ml](lib/ast.ml))
4. **MLsem AST** - Final representation for type inference via `Ast.to_mlsem`

**Key transformation chain:**

```
C file → CST (Parse.ml) → PAst (parser.ml:to_ast)
      → Ast (PAst.transform) → MLsem (Ast.to_mlsem) → Type Inference
```

See [lib/runner.ml](lib/runner.ml) `run_on_file`, `run_on_files`, and `infer_def` for the complete pipeline execution.

## Critical Module Responsibilities

- **[lib/parser.ml](lib/parser.ml)** - Converts tree-sitter CST to PAst; handles C syntax edge cases
- **[lib/PAst.ml](lib/PAst.ml)** - Pre-typed AST; `transform` converts string identifiers to `Variable.t`
- **[lib/ast.ml](lib/ast.ml)** - Typed AST with `kind VarMap.t` tracking constant/label info; `to_mlsem` conversion
- **[lib/runner.ml](lib/runner.ml)** - Orchestrates entire pipeline; handles type inference via mlsem System
- **[lib/defs.ml](lib/defs.ml)** - R C API type definitions; `initial_env` contains builtin types
- **[lib/type_parser.ml](lib/type_parser.ml)** - Parses `.ty` files using rstt syntax (`symbol: type` or `symbol = type`)

## Critical Dependencies (Not in Public Registries)

The project requires **private GitHub repositories** that must be pinned with authenticated access:

- **MLsem**: Core type inference framework (`mlsem-types`, `mlsem-common`, `mlsem-system`, etc.) from `E-Sh4rk/MLsem`
- **SSTT/RSTT**: Set-theoretic type system (`sstt`, `sstt-repl`, `rstt`, `rstt-repl`) from `E-Sh4rk/sstt`
- **r-parser**: Tree-sitter C parser from `E-Sh4rk/r-parser`

Pin with: `opam pin add <pkg> git+https://<PAT>@github.com/E-Sh4rk/<repo>.git --yes`

**cmdliner version**: Must use exactly `1.0.4` due to API changes in later versions.

Project dependencies are also specified in:

- [r-c-typing.opam](r-c-typing.opam) - OCaml package dependencies (mlsem, sstt, rstt, cmdliner)
- [dune-project](dune-project) - Dune build configuration and package metadata

## Type System Integration

**External type definitions** live in `types/base.ty` and `types/posix.ty` using rstt syntax:

- Format: `function_name: t(arg_types) -> return_type`
- Aliases: `name = type_expression`
- Intersection types: `(type1) & (type2)`
- Examples: `isInteger: (t(v(int)) -> c_true) & (t(~v(int)) -> c_false)`

The rstt library provides R-specific type constructors:

- Vectors: `v[length](element_type)` e.g., `dbl1` = double vector of length 1
- Lists: `{label:type; tail}` for record-like list shapes with a typed tail, e.g. `{foo:int; chr | v[2](dbl)}`
- Primitives: `p(element_type)` for R primitive values
- C types: `c_int`, `c_double`, `c_string`, `c_bool` (with refinements like `c_true`)
- Set-theoretic operators: `&` (intersection), `|` (union), `~` (negation)

**Critical distinction for type definitions:**

- **[types/base.ty](types/base.ty)** - For types expressible in the rstt type algebra
- **[lib/defs.ml](lib/defs.ml)** - For custom type constructors that create function-specific types (e.g., list creation with specific labels, since labels aren't types themselves)

**Type parsing**: [lib/type_parser.ml](lib/type_parser.ml) reads `.ty` files, distinguishes definitions (`:`) vs aliases (`=`)

**Initial environment**: [lib/defs.ml](lib/defs.ml) loads base types and R C API function signatures into `Defs.initial_env`

**Type precedence for one symbol**:

- First: types loaded from `.ty` files
- Second: types inferred from a full function definition with a body
- Third: types inferred from a declaration or other signature-only/simple C inference

Operational rule: `.ty` definitions must never be overridden by inferred C types; a full function body may replace an earlier declaration-derived type; a declaration-derived type is only a fallback when no `.ty` definition or body-based type is available.

## Development Workflows

### Environment Setup

**Required before building:**

Source `setup-env.sh`, which derives the tree-sitter paths from an `r-parser`
checkout. It defaults to `../r-parser` (next to this repository); set
`R_PARSER_PATH` if yours lives elsewhere. `make build` / `make test` source it
for you.

```bash
source ./setup-env.sh                                # default ../r-parser
R_PARSER_PATH=/path/to/r-parser source setup-env.sh  # elsewhere
```

Do not run `opam install . --deps-only` in a switch set up this way: it
replaces the local `tree-sitter` with the public opam package, which has no
`run` sublibrary, and the build then fails with `Library "tree-sitter.run" not
found`. See the "Tree sitter" section of [README.md](../README.md) for why and
how to recover.

See [.github/workflows/ci.yml](.github/workflows/ci.yml) for CI setup patterns.

### Build & Test

```bash
dune build                    # Build entire project
dune runtest                  # Run all tests (compares against .expected files)
dune build test/typing_test.exe  # Build standalone test executable
dune exec bin/main.exe -- <file.c>  # Run type inference on a C file
```

### Large-Change Validation

For substantial parser, preprocessor, PAst, runner, or package-handling changes, do not stop at `dune build` or `dune runtest`.

- Validate the change against the external pipeline in the `r-typing` repository, checked out next to this one (`../r-typing`).
- Run the pipeline on at least one small real package such as `clue` or `jsonlite` via `scripts/run_one_package.sh`.
- Treat package-level crashes, new nonzero exits, decreases in the number of typed functions for a package, or other obvious regressions in the pipeline output as blocking until investigated.

Example:

```bash
CHECKER_DIR="$PWD"          # this repository, holding _build/default/bin/main.exe
cd ../r-typing
CHECKER_DIR="$CHECKER_DIR" scripts/run_one_package.sh work/sources/jsonlite work/raw_output_test
CHECKER_DIR="$CHECKER_DIR" scripts/run_one_package.sh work/sources/clue work/raw_output_test
```

Pass `CHECKER_DIR` explicitly: `r-typing`'s own defaults (`Makefile` and
`scripts/run_one_package.sh`) still point at paths from before this repository
was renamed and moved, so omitting it silently runs the wrong checker or none.

### Code Coverage Analysis

The project uses bisect_ppx for code coverage instrumentation:

```bash
# Run tests with coverage instrumentation
dune clean
mkdir -p _coverage
env BISSECT_COVERAGE_OUTPUT=_coverage dune runtest --instrument-with bisect_ppx

# Generate HTML coverage report
bisect-ppx-report html

# View report at _coverage/index.html
```

### Adding New Tests

**Use the provided script** to automate test creation:

```bash
./add_test.sh myfunction     # For test/cprogs/myfunction.c
```

This script:

1. Generates `myfunction.expected` by running `typing_test.exe`
2. Appends dune rules to `test/dune.inc` for automated diff testing
3. Tests compare stdout output against `.expected` files

**Manual test structure:**

- Place C file in `test/cprogs/`
- Expected output shows: `function_name: inferred_type`
- Test failures show diffs between actual and expected types

### Debugging Type Inference

Command-line flags control output verbosity:

```bash
dune exec bin/main.exe -- --cst file.c      # Show concrete syntax tree
dune exec bin/main.exe -- --past file.c     # Show parsed AST
dune exec bin/main.exe -- --ast file.c      # Show typed AST
dune exec bin/main.exe -- --mlsem file.c    # Show MLsem AST
dune exec bin/main.exe -- --no-typing file.c # Skip type inference
dune exec bin/main.exe -- --debug file.c    # Enable debug mode
dune exec bin/main.exe -- -f substr file.c  # Filter to functions matching substr
```

Combine flags to trace transformations: `--past --ast --mlsem`

Additional debugging tips:

- Pass `--mlsem-recording` to dump MLsem type-system call traces to `mlsem_recording.json` (off by default, since the dump adds several seconds and tens of MB per package)
- Tree-sitter parse errors appear in CST output - use `--cst` to diagnose parsing issues

## Inference Pipeline ([lib/runner.ml](lib/runner.ml))

1. Parse C file → CST ([lib/parser.ml](lib/parser.ml))
2. Convert CST → PAst (untyped, position-annotated)
3. Transform PAst → Ast via `PAst.transform` - converts string IDs to `Variable.t`, handles scoping
4. Convert Ast → MLsem AST via `Ast.to_mlsem`
5. Type inference via MLsem: `System.Refinement.refinements` → `System.Reconstruction.infer` → `System.Checker.typeof_def`
6. Output normalized type signature: `TyScheme.norm_and_simpl`

**Environment threading**: Functions accumulate typed signatures in `(idenv, env)` tuple - `idenv` maps function names to Variables, `env` maps Variables to types.

## Project-Specific Conventions

### Variable Handling

- **PAst** uses `string` identifiers
- **Ast** uses `Mlsem.Lang.MVariable.t` created via `MVariable.create Immut (Some name)`
- Transform maintains `idenv: string -> Variable.t` mapping (see [lib/PAst.ml](lib/PAst.ml) `transform`)
- **Immut** mutability required for function-scoped variables

### Position Tracking

All AST nodes carry `Position.t` from `Mlsem.Common`:

- Parsed from tree-sitter `Loc.t` via `loc_to_pos` in [lib/parser.ml](lib/parser.ml)
- Use `Position.dummy` when position is unavailable
- Enables precise error reporting in type inference

### Type Environment

The typing environment (`Env.t`) maps `Variable.t -> TyScheme.ty`:

- Initialize with `Defs.initial_env` containing R C API builtins
- `Runner.extend_env` adds `dyn` type for missing free variables
- Upper bound extraction: `TyScheme.get` then `GTy.ub` in the body-inference path in [lib/runner.ml](lib/runner.ml)

### Test File Structure

**Auto-generated** `test/dune.inc` contains rules for each test:

```dune
(rule (alias runtest)
  (action (with-stdout-to func.output
    (run %{exe:typing_test.exe} %{dep:cprogs/func.c}))))
(rule (alias runtest)
  (action (diff cprogs/func.expected func.output)))
```

Never edit `test/dune.inc` manually - regenerate via `add_test.sh`.

## Common Patterns

### Adding R C API Function Types

1. **Types expressible in the type algebra:** Add to [types/base.ty](types/base.ty)

   ```
   myFunc: t(input_type) -> output_type
   ```

   Example: Simple function signatures with standard R types

2. **Custom type constructors:** Define in [lib/defs.ml](lib/defs.ml) `BuiltinVar` module using `Rstt.Builder`
   - Use for functions requiring dynamic type construction
   - Typical case: List creation/update functions where labels aren't types themselves
   - Examples: `allocVector_vecsxp_ty`, `mkNamed_vecsxp_ty`, `set_vector_elt_ty`

3. Types are loaded into `initial_env` via `Type_parser.load_signatures`

### Handling Tree-Sitter Parsing

C parser located in `c-parser/` with FFI bindings:

- `Parse.file` and `Parse.string` entry points
- Error handling via `Parsing_result.errors`
- Custom CST module generated by tree-sitter

### Working with MLsem

MLsem provides the type inference engine:

- `System.Refinement.refinements` - computes type refinements
- `System.Reconstruction.infer` - performs type reconstruction
- `System.Checker.typeof_def` - extracts inferred type
- All functions require typing environment `env: Env.t`
- Type errors caught as `System.Checker.Untypeable` exceptions

Configuration: `System.Config.infer_overload := true` enables overload resolution.

## Common Pitfalls

- **Private dependencies** - Don't assume public opam packages; MLsem/SSTT require authenticated GitHub access
- **cmdliner version** - Don't use newer cmdliner APIs; the project is locked to 1.0.4
- **Missing tree-sitter env vars** - Build fails if `TREESITTER_INCDIR` and `TREESITTER_LIBDIR` are not set correctly
- **PAst vs Ast confusion** - PAst has strings, Ast has Variables; use appropriate type
- **Type parser inline comments** - Use `//` not `/* */` in `.ty` files
- **dune.inc out of sync** - Regenerate with `add_test.sh` rather than manual edits
- **Position.dummy overuse** - Prefer real positions from tree-sitter for better errors
- **Flushing output** - Don't forget to flush stdout after tests (see [test/typing_test.ml](test/typing_test.ml))

## Key Files Reference

- [bin/main.ml](bin/main.ml) - CLI entry point with cmdliner
- [lib/runner.ml](lib/runner.ml) - Core type inference orchestration
- [lib/defs.ml](lib/defs.ml) - R C API type definitions and initial environment
- [lib/parser.ml](lib/parser.ml) - CST to PAst conversion
- [lib/type_parser.ml](lib/type_parser.ml) - Parse `.ty` type definition files
- [test/typing_test.ml](test/typing_test.ml) - Test harness for expectation tests
- [types/base.ty](types/base.ty) - R C API function type signatures
