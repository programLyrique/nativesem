#include <Rinternals.h>

/* Forward gotos: the statements before the label become a block that the
 * goto leaves, including from inside a loop. A backward goto is a no-op. */
int f(int x);
int plain(int x) { int err = 0; if (x < 0) { err = 1; goto exit; } err = f(x); exit: return err; }
int from_loop(int n) { int i; for (i = 0; i < n; i++) { if (f(i)) goto done; } return -1; done: return i; }
int two(int x) { int r = 0; if (x == 1) goto one; if (x == 2) goto second; r = 3; goto end; one: r = 1; goto end; second: r = 2; end: return r; }
int backward(int n) { int i = 0; again: i++; if (i < n) goto again; return i; }
SEXP cleanup(int n) { SEXP out = PROTECT(Rf_allocVector(INTSXP, n)); if (n < 0) goto fail; UNPROTECT(1); return out; fail: UNPROTECT(1); return R_NilValue; }

/* A macro with a goto is expanded (utf8's TRY). */
#define TRY(x) do { if ((err = (x))) { goto exit; } } while (0)
int use_try(int x) { int err = 0; TRY(f(x)); TRY(f(x + 1)); return 0; exit: return err; }
