#include <Rinternals.h>

/* [x == c] is the type test [x : typeof(c)] only when that type has a single
   value. A char, a double or a global SEXP has a type with many values: the
   comparison is then a [c_bool], in both branches. */

SEXP strings_dots;

int char_eq(char c) { if (c == 'a') return 1; return 2; }
int char_neq(char c) { if (c != 'a') return 1; return 2; }
int double_eq(double d) { if (d == 1.5) return 1; return 2; }
int global_eq(SEXP s) { return s == strings_dots; }
int env_eq(SEXP s) { return s == R_GlobalEnv; }

/* A single value: an enumerator, NA, NULL. */
enum colour { RED, GREEN };
int enum_eq(enum colour c) { if (c == GREEN) return 1; return 2; }
int na_int_eq(int x) { return x == NA_INTEGER; }
int na_string_neq(SEXP s) { return s != NA_STRING; }
int nil_eq(SEXP s) { if (s == R_NilValue) return 1; return 2; }
