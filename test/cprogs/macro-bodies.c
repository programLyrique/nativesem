#include <Rinternals.h>

/* A macro body is parsed inside a function: as an expression first (the macro
   returns its value), else as a statement. Each of these used to be parsed as
   a standalone translation unit and, except the bare identity, bound at
   any -> any. */
#define KEEP(x) PROTECT(x)
#define ID(x) (x)
#define TWICE(x) (x) + (x)
#define SWAP(a, b) do { int t = a; a = b; b = t; } while (0)

SEXP use_keep(SEXP x) { SEXP y = KEEP(x); UNPROTECT(1); return y; }
int use_id(int x) { return ID(x); }
int use_twice(int x) { return TWICE(x); }

/* Not C: still bound at any -> any. */
#define NOT_C(x) x x x )

/* Bodies whose meaning depends on the expansion site (the caller's labels,
   loops and locals) are not functions: a call is replaced by the body, the
   arguments substituted for the parameters, as C does. TRY is not: goto is
   not modeled, so it stays unbound. */
#define TRY(x) do { if ((err = (x))) { goto exit; } } while (0)
#define CHECK(x) if (!(x)) return -1
#define STOP_IF(x) if (x) break
#define BUMP() (count++)
/* Loops and switches of the body itself, locals of the body: still functions. */
#define FIRST_POS(p, n, out) do { int i; for (i = 0; i < n; i++) { if (p[i] > 0) break; } out = i; } while (0)

int use_try(int x) { int err = 0; TRY(x); exit: return err; }

/* A global declared before the macro is visible where it is defined: assigning
 * it does not depend on the expansion site (rlang's r_preserve). */
static SEXP placeholder = NULL;
#define PRESERVE(X) (R_PreserveObject(placeholder = X), (void) NULL)
void use_preserve(SEXP x) { PRESERVE(x); }

int use_check(int x) { CHECK(x > 0); return x; }
int use_stop(int n) { int i = 0; while (1) { STOP_IF(i >= n); i++; } return i; }
#define CALL_ERR(f, ...) do { if (f(__VA_ARGS__)) return -1; } while (0)
int g2(int a, int b);
int use_call_err(int a, int b) { CALL_ERR(g2, a, b); return 0; }
