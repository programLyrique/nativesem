#include <Rinternals.h>

/* Typed with --fallback-c-signature (see test/dune): a recursive function is
   bound at its declared C signature before its body is typed, so a recursive
   call sees that signature instead of an unbound name. */
int fact(int n) { if (n <= 1) return 1; return n * fact(n - 1); }

int sum_to(int n) { return n <= 0 ? 0 : n + sum_to(n - 1); }

/* A caller of a recursive function. */
int fact5(void) { return fact(5); }

/* The rest of the body is still inferred: the inferred types are finer than
   the signatures [any_sexp] --> c_int_na and [any_sexp] --> any_sexp. */
int depth(SEXP x) { if (TYPEOF(x) != LISTSXP) return 0; return 1 + depth(CDR(x)); }
SEXP last(SEXP x) { if (x == R_NilValue) return x; if (CDR(x) == R_NilValue) return CAR(x); return last(CDR(x)); }
