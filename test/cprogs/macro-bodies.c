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
   arguments substituted for the parameters, as C does. */
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

/* The summaries (typing_test --summaries) of rlang's helpers (vctrs'
   rlang/attrib.h): a macro and inline functions that change attributes. */
struct r_symbols { SEXP names; SEXP klass; };
extern struct r_symbols r_syms;
#define r_attrib_poke_names(X, VALUE) Rf_setAttrib(X, r_syms.names, VALUE)
static inline void r_attrib_zap(SEXP x, SEXP tag) { Rf_setAttrib(x, tag, R_NilValue); }
static inline void r_attrib_clone_from(SEXP to, SEXP from) { SHALLOW_DUPLICATE_ATTRIB(to, from); }
SEXP use_poke_names(SEXP x, SEXP nms) { r_attrib_poke_names(x, nms); return x; }
void use_zap(SEXP x) { r_attrib_zap(x, R_NamesSymbol); }
void use_clone(SEXP x, SEXP y) { r_attrib_clone_from(x, y); }

/* A macro that changes a variable of the code it is expanded in. */
#define NAME_OUT(v) Rf_setAttrib(out, R_NamesSymbol, v)
SEXP use_name_out(SEXP out, SEXP v) { NAME_OUT(v); return out; }

/* A macro's summary at its calls: the names of a value of class "a" are set,
   its class is kept. */
SEXP poke_names_keeps_class(SEXP nms)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  r_attrib_poke_names(l, nms);
  UNPROTECT(1);
  return l;
}
