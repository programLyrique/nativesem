// The empty statement `;` does nothing. It used to be translated as
// `return;`, so that a `for` loop without an init clause returned from the
// function before the loop, and the function was typed as returning nothing.

#include <Rinternals.h>

// vctrs' ffi_as_minimal_names: an empty init, a break, a return after.
SEXP first_na_or_all(SEXP names) {
  if (TYPEOF(names) != STRSXP) error("`names` must be a character vector");
  R_xlen_t i = 0;
  R_xlen_t n = XLENGTH(names);
  for (; i < n; ++i) {
    if (STRING_ELT(names, i) == NA_STRING) break;
  }
  if (i == n) return names;
  return R_NilValue;
}

// No init and no increment.
SEXP count_down(SEXP x) {
  R_xlen_t n = XLENGTH(x);
  for (; n > 0;) n--;
  return x;
}

// No clause at all.
SEXP forever(SEXP x) {
  int i = 0;
  for (;;) {
    if (++i > 3) break;
  }
  return x;
}

// Stray empty statements.
SEXP stray(SEXP x) {
  ;
  if (x == R_NilValue) ;
  return x;
}
