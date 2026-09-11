# R C typing

[![CI](https://github.com/programLyrique/r-c-typing/actions/workflows/ci.yml/badge.svg)](https://github.com/programLyrique/r-c-typing/actions/workflows/ci.yml)

Type functions written in C using the R C API to R types.

## Dependencies

- OCaml >= 5.3.0
- dune
- mlsem
- sstt
- rstt
- cmdliner

You can install the OCaml libraries using `opam`.

`mlsem`, `sstt`, and `rstt` are only on GitHub so far but can be installed with `opam pin`.

## Tree sitter

You need to compile and install `https://github.com/E-Sh4rk/r-parser`.

`setup-env.sh` (which the `Makefile` sources for you) derives the tree-sitter
paths from it. By default it looks for r-parser next to this repository, at
`../r-parser`; set `R_PARSER_PATH` if it lives elsewhere:

```bash
R_PARSER_PATH=/path/to/r-parser source setup-env.sh
```

It exports the three variables the build needs:

```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$R_PARSER_PATH/core/tree-sitter/lib
export TREESITTER_INCDIR=$R_PARSER_PATH/core/tree-sitter/include
export TREESITTER_LIBDIR=$R_PARSER_PATH/core/tree-sitter/lib
```

### `opam install --deps-only` will break the build

`r-parser/core/tree-sitter.opam` declares empty `build:`/`install:` sections, so
`opam pin add tree-sitter core/` only registers the package to satisfy the
dependency -- it installs nothing. The switch entry actually comes from
r-parser's own `make install` (`dune install` in `core/`).

The consequence: `opam install . --deps-only` resolves `tree-sitter` from
opam.ocaml.org and overwrites that entry. The public package has `json` and
`ocaml` sublibraries but no `run`, so the next build fails with

```
File "c-parser/dune", line 7, characters 28-43:
Error: Library "tree-sitter.run" not found.
```

Re-pinning alone does not fix it (the pin installs nothing). Reinstall from
r-parser instead:

```bash
cd $R_PARSER_PATH/core && dune build && dune install
```

To check which one you have, look for the package metadata -- the local install
writes a `dune-package`, and `tree-sitter.run` should be listed:

```bash
ls $(opam var lib)/tree-sitter/dune-package && ocamlfind list | grep tree-sitter
```

## Common commands

The repository includes a `Makefile` that wraps the required environment setup:

```bash
make build
make test
make release
make coverage
```

- `make build` runs `dune build`
- `make test` runs `dune runtest`
- `make release` runs `dune build --profile release`
- `make coverage` runs the test suite with `bisect_ppx`, stores raw coverage data in `_coverage/data/`, and writes the HTML report to `_coverage/html/index.html`

## Adding type inference tests

Use the provided helper script so the test list stays in sync.

```bash
./add_test.sh myfunction
```

This creates `test/cprogs/myfunction.c`, generates `test/cprogs/myfunction.expected` using the current inference output, and updates `test/dune.inc` with the corresponding rules. Then run the tests with:

```bash
dune runtest
```

Avoid editing `test/dune.inc` manually. Re-run `add_test.sh` if you need to refresh the expected output.

## CLI examples

The positional argument is a `PATH`: either a C source file or an R package directory.

Header lookup can also be extended with repeated `-I/--include-dir` flags or with the `C_INCLUDE_PATH` environment variable.

Show the parsed AST for functions whose name contains "from":

```bash
dune exec r-c-typing -- --past -f from test/cprogs/string.c
```

For a single C file, `-f/--filter` is an output filter: non-matching
functions may still be processed so later functions can use their types, but
only matching symbols are printed.

Run inference for a single file:

```bash
dune exec r-c-typing -- test/cprogs/string.c
```

Run inference for an R package directory:

```bash
dune exec r-c-typing -- test/packages/testpkg
```

For an R package directory, the default root set is the package's native
entry points discovered from `.C`, `.Call`, and `.External` calls. Passing
`-f/--filter SUBSTRING` changes the root set to all package C functions whose
names contain `SUBSTRING`; only those roots and their transitive callees are
typed and printed.

For example, this analyzes just the closure rooted at matching functions:

```bash
dune exec r-c-typing -- -f r_is_scalar_logical path/to/vctrs
```

## Useful flags

| Flag                                  | Description                                                                                                                                                                                                                                                                                                              |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `--timeout SECONDS`                   | Per-function timeout for body inference. The rest of the package continues after a timeout.                                                                                                                                                                                                                              |
| `--fallback-c-signature`              | When body inference fails (untypeable, internal error, or timeout), bind the function at its declared C signature — `params -> ret_ty` — so callers can still be typed instead of cascading as "unbound variable". The original error is still printed, followed by an indented `fallback: <type>` line. Off by default. |
| `--log-times`                         | Emit timing diagnostics. Two kinds of lines: (1) an indented `  timing: <seconds> s` line below every function's result giving the wall-clock time of its body inference (indentation matches the `fallback:` convention so the per-function CSV format is unaffected); (2) coarse `Phase: <name> <seconds> s` lines for the major pipeline stages — `load_ty`, `parsing`, `call_graph`, `non_fundef_pass`, `fundef_pass`, `recording_save` (the last only when `--mlsem-recording` is also set). The `Phase:` prefix is filtered as noise by the `r-typing` CSV parser. Off by default.                                |
| `--mlsem-recording`                   | Bracket the run with `Mlsem.Types.Recording.start_recording` / `save_to_file` and dump a per-process call tally to `mlsem_recording.json` in the current directory. The dump can be several tens of MB and add several seconds per package, so it is off by default — including for the CRAN pipeline. Turn on to inspect mlsem-side call statistics.                                                                                                                                                                                                                |
| `--debug`                             | Print intermediate AST/inference information.                                                                                                                                                                                                                                                                            |
| `--past`, `--ast`, `--mlsem`, `--cst` | Print the corresponding intermediate form.                                                                                                                                                                                                                                                                               |
| `-f/--filter SUBSTRING`               | For a single C file, restrict printed output to symbols whose name contains `SUBSTRING`. For a package directory, use matching package C functions as call-graph roots and type/print only those roots and their transitive callees.                                                                                     |

## Type definitions

Builtin and external signatures are currently described in both `types/base.ty` and `types/posix.ty`.

The rstt syntax supports, among others:

- Vectors: `v(element_type)` for any length, `v1(element_type)` for a scalar.
  Vector lengths are not part of the type algebra, and only a scalar may carry a
  refined element type (a string/number singleton, an `^` NA-free component);
  the element type of a non-scalar vector has to be a whole mode.
- Element modes: `INT`, `DBL`, `LGL`, `CHR`, `CLX`, `RAW` denote one mode
  exactly, while the lowercase spellings (`int`, `dbl`, ...) additionally
  include every mode that coerces up to it — so `p(chr)` is all of `prim`,
  and a CHARSXP is `p(CHR)`.
- Tuples: `[t1, t2]` — this is how the arguments of a C function are passed
  (written `t(t1, t2)` in older versions of the syntax).
- Lists with typed tails: `{label:type; tail}`
- Intersections and unions: `t1 & t2`, `t1 | t2`

See the header comment of `types/base.ty` for more on these conventions.

## Type precedence

When several type sources exist for the same symbol, the runner keeps the following precedence:

1. Types loaded from `.ty` files.
2. Types inferred from a full function definition with a body.
3. Types inferred from a declaration or signature-only C function.

In practice, this means:

- A symbol already defined in `types/*.ty` is never overridden by inferred C information.
- A later full function definition overrides a previously inferred declaration for the same symbol.
- If no body is available, the declaration or simple C signature is kept and used as the symbol type.
