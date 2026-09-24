# ─── Stage 1: Build ──────────────────────────────────────────────────────────
FROM ocaml/opam:ubuntu-22.04-ocaml-5.3 AS builder

USER root
RUN apt-get update && apt-get install -y \
    build-essential curl bc git \
    libgmp-dev pkg-config \
    libssl-dev \
    nodejs \
    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam

# Rust is required by tree-sitter-cli during the r-parser build.
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
      | sh -s -- -y --no-modify-path
ENV PATH="/home/opam/.cargo/bin:${PATH}"

RUN opam update --yes && eval $(opam env) && \
    opam install ocamlfind dune cmdliner.1.0.4 --yes

# Known-good pins of the set-theoretic stack. This RSTT is the length-free
# vector representation (Vec.Vector / Vec.Scalar); the types/*.ty files and
# lib/defs.ml are written against it. Keep in sync with .github/workflows/ci.yml.
ARG SSTT_REF=b44200579ebf2522da24d7d7105bbf1699d1e9d2
ARG MLSEM_REF=7fae576697b3cf29e9331273d8a98ca797c9cfd3
ARG RSTT_REF=8b197ea9366caff98c6460b1ef4801b72c77dfbb

# sstt, MLsem, and RSTT are now public — no PAT needed.
RUN eval $(opam env) && \
    opam pin add sstt      "git+https://github.com/E-Sh4rk/sstt.git#${SSTT_REF}"   --yes --no-action --ignore-pin-depends && \
    opam pin add sstt-repl "git+https://github.com/E-Sh4rk/sstt.git#${SSTT_REF}"   --yes --no-action --ignore-pin-depends && \
    opam pin add sstt-bin  "git+https://github.com/E-Sh4rk/sstt.git#${SSTT_REF}"   --yes --no-action --ignore-pin-depends && \
    for pkg in mlsem-types mlsem-common mlsem-system mlsem-lang \
               mlsem mlsem-app mlsem-bin; do \
      opam pin add "$pkg" "git+https://github.com/E-Sh4rk/MLsem.git#${MLSEM_REF}" --yes --no-action --ignore-pin-depends; \
    done && \
    opam pin add rstt      "git+https://github.com/E-Sh4rk/rstt.git#${RSTT_REF}" --yes --no-action --ignore-pin-depends && \
    opam pin add rstt-repl "git+https://github.com/E-Sh4rk/rstt.git#${RSTT_REF}" --yes --no-action --ignore-pin-depends && \
    opam pin add rstt-bin  "git+https://github.com/E-Sh4rk/rstt.git#${RSTT_REF}" --yes --no-action --ignore-pin-depends

# r-parser is public; clone it directly without injecting credentials.
RUN git clone "https://github.com/E-Sh4rk/r-parser.git" r-parser && \
    cd r-parser && \
    sed -i 's|git@github.com:|https://github.com/|' .gitmodules && \
    git submodule sync && \
    export CARGO_TARGET_DIR="$PWD/core/downloads/tree-sitter/target" && \
    opam exec -- bash -c "make update && make setup" && \
    opam exec -- bash -c "make" && \
    cd core && \
    opam exec -- bash -c "opam pin add tree-sitter . --kind=path --yes" && \
    opam exec -- dune install --prefix=$(opam var prefix)

ENV TREESITTER_INCDIR="/home/opam/r-parser/core/tree-sitter/include"
ENV TREESITTER_LIBDIR="/home/opam/r-parser/core/tree-sitter/lib"
ENV LD_LIBRARY_PATH="/home/opam/r-parser/core/tree-sitter/lib"

COPY --chown=opam:opam . /home/opam/r-c-typing/
WORKDIR /home/opam/r-c-typing
RUN eval $(opam env) && \
    opam install . --deps-only --ignore-pin-depends --yes && \
    dune build

# ─── Stage 2: Runtime ────────────────────────────────────────────────────────
# Only binary artifacts are copied here — no opam metadata, no source,
# no git history, no PAT can reach this stage.
FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC

RUN apt-get update && apt-get install -y \
    libstdc++6 bc git \
    r-base parallel \
    && rm -rf /var/lib/apt/lists/*

# Keep the _build/default/bin/main.exe path that run_one_package.sh expects.
RUN mkdir -p /checker/_build/default/bin
COPY --from=builder \
    /home/opam/r-c-typing/_build/default/bin/main.exe \
    /checker/_build/default/bin/main.exe

# Type definitions read at runtime relative to CHECKER_DIR (CWD of the checker).
COPY --from=builder /home/opam/r-c-typing/types /checker/types

# tree-sitter shared library.
COPY --from=builder \
    /home/opam/r-parser/core/tree-sitter/lib/libtree-sitter.so* \
    /usr/local/lib/
RUN ldconfig

# These variables are consumed by scripts/run_one_package.sh in r-typing.
ENV CHECKER_DIR=/checker
ENV CHECKER=/checker/_build/default/bin/main.exe
ENV TS_LIB_DIR=/usr/local/lib

WORKDIR /checker
