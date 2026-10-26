# Sources of unsoundness

The places where NativeSem can give a C function a type that some execution
violates: a result type missing values the function can return, a domain
accepting arguments on which it fails, or code that is not typed at all. Each
entry says where it happens and what goes wrong. Deliberate trust in external
specifications (the `.ty` files, the C types) is listed at the end.

Approximations that only lose precision (an `any` where something finer would
hold, a function bound at its C signature instead of an inferred type) are not
listed, except when they interact with one of the entries below.

## Control flow

- **Backward `goto`s are no-ops.** A forward `goto` leaves the block built
  for its label (`PAst.label_blocks`), but a `goto` to a label before it, or
  into a block that does not contain it, is lowered to a null constant: the
  code after it is typed as if execution fell through, and a loop made of a
  backward `goto` is not a loop (the values reaching its label along the back
  edge are not considered).
- **`case` on an unknown constant matches every integer.** In a `switch`
  (`Ast.aux_e`, `Switch`), a case label that is neither a literal nor a known
  constant gets the pattern `c_int_na`. Patterns are tried in order, so the
  cases after it are seen as unreachable and their bodies are not typed.
- **`continue` is lowered as `break`** (`Ast.aux_e`: `Next -> A.Break`). The
  rest of the iteration and the next iterations are not followed from a
  `continue`. Probably harmless with MLsem's mutable variables (a loop body is
  typed once), but not checked.

## Memory and mutation

C memory is not modeled. A variable holding a pointer, a struct, or an R
object gets a type; what other variables or other memory say about the same
object is not kept in sync.

- **Writes through a pointer are not seen by later reads.** `*p = v` is
  lowered to an assignment to a fresh local (`PAst.aux_e`, `VarAssign` of a
  `Unop`); `p->f = v` gives a new struct value that is discarded. A later
  `*p` or `p->f` still has the type it had before the write, including what
  inference assumed of it through the parameter's type variable.
- **`arr[i] = v` rebinds `arr` only.** It is modeled as `arr = []<-(arr, i, v)`
  (strong update of the variable). Another pointer to the same array keeps
  the old element type.
- **SEXP fields of structs are refined as if they did not change.** A struct's
  type carries the record of its SEXP fields (`Ast.Structs`), which inference
  refines (`struct poly_vec({ vec : INT })`). A write to the field through
  another pointer, or by a callee, is not reflected: the refined type is a
  precondition at entry, not an invariant. Writing the field through the same
  variable resets it to `any_sexp`.
- **In-place mutation of R objects updates one variable.** `setAttrib`,
  `SET_VECTOR_ELT` and `SET_STRING_ELT` are modeled as `v = f(v, ...)` when
  their first argument is a variable (`PAst.process_call`): other variables,
  struct fields or list elements holding the same object keep their old type,
  and so does the object when the first argument is not a variable
  (`setAttrib(p->vec, ...)`). The other mutators of the C API (`SETCAR`,
  `SET_TAG`, `SET_ATTRIB`, `SET_TYPEOF`, `SETLENGTH`, `Rf_classgets`, ...)
  are not modeled at all: a class or length set through them is not seen.
- **Union members are read at their declared type**, whichever member was
  written last (C's type punning).

## C types and values

- **All integer types are `c_int_na`.** Width, signedness and overflow are
  ignored (`aux_primitive_type`, `typeof_ctype`), including for `size_t`,
  `uint64_t`, ... Literal and range types (`c(1..)`, `c(-1)`) derived by
  arithmetic assume no wrap-around: an unsigned subtraction or an overflow
  can produce a value outside the inferred range.
- **Explicit casts are trusted.** `(T) e` is a coercion without check
  (`Ast.aux_e`, `Cast` → `TypeCoerce NoCheck`): the result has type `T`
  whatever `e` is, e.g. `(SEXP) R_ExternalPtrAddr(p)`.
- **The implicit `void*` → `T*` conversion intersects.** At an initialization
  or an assignment to a variable declared `T*`, the value is narrowed to
  `T*` (`CoerceNarrow` → `TypeCast`, an intersection). If the value cannot be
  a `T*` at all (a `char*` assigned to an `int*` without a cast), the
  intersection is empty and the code after it is considered unreachable, so
  it is not typed.
- **Struct field writes are not checked** against the field's declared type
  (`Ast.field_write_scheme`), since C's implicit conversions are not modeled;
  reads give the declared type. A write of an incompatible value (possible
  with casts) makes later reads wrong.

## Macros and preprocessing

- **Function-like macros are typed as functions.** A macro whose body behaves
  like a function body (`Preprocessor.behaves_like_function`) is bound as one
  (the others are expanded at their calls, `Preprocessor.expand_macro`, which
  is C's semantics):
  its arguments are evaluated once, before the body, and the names in its
  body are resolved where the macro is defined. At an expansion they are
  evaluated as many times as they occur, possibly not at all, and the body's
  free names are resolved at the expansion site: a caller's local that
  shadows a global the body uses (or assigns, which the check allows for
  globals declared before the macro) is not the variable that was typed.
- **Object-like macros are not expanded before parsing.** Tree-sitter parses
  the source as written. A macro standing for an attribute or a qualifier
  (rlang's `r_unused`, R's `attribute_hidden`) is taken for the type of the
  declaration it starts, and the real type is dropped by the error recovery:
  `static r_unused r_obj* _r_placeholder` declares a `*any`.
- **`#if` branches are chosen from the defines NativeSem knows.** The branch
  taken may not be the one the package is built with (an unknown macro is
  undefined, or 0), and the code of the other branches is not typed.
- **Code that tree-sitter cannot parse is dropped.** Error recovery removes
  nodes (system headers report "N node(s) dropped"): declarations may be
  missing or, as above, wrong.

## Inference

- **Type-case exploration keeps a branch only when it types.** With
  overloaded inference (`infer_overload`), a branch of the function's domain
  whose typing fails is dropped from the intersection, and a function may end
  up with only its error path (`[c_null] -> empty`). This is sound for the
  domain kept, but the domain shown is then narrower than the C signature;
  callers outside it fail instead of being typed. A8's completion
  (`completed by the C signature`) fills the rest of the domain with the C
  signature's result.

## Trusted specifications

These are assumptions rather than defects; a mistake in them propagates.

- **The `.ty` signatures** (`types/base.ty`, `posix.ty`, `winapi.ty`) and the
  `typedef` lines there (undeclared typedefs such as `R_xlen_t` take that
  type).
- **The declared C types**: parameter and return types, struct field types,
  function pointer types (`(DL_FUNC) &f`), `noreturn` attributes. They are
  used as fallbacks (`--fallback-c-signature`, timeouts, A8's completion, the
  pre-binding of recursive functions) and for field reads.
- **Values from unparsed headers**: an unresolved type is `any`, an unknown
  constant `c_int_na`.
