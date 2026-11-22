#include <Rinternals.h>

/* The escapes of R values (lib/PAst.ml): the value l gets the class "a",
   then something happens to it. When it escapes, its classes and attributes
   are forgotten (only its content is kept); otherwise the class is kept. */

static SEXP helper(SEXP x);
struct holder { SEXP value; };

/* Read by functions of types/base.ty, protected: kept. */
SEXP kept_read(void)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  if (LENGTH(l) > 1) Rf_error("no");
  UNPROTECT(1);
  return l;
}

/* Re-protected in place: kept. */
SEXP kept_reprotect(void)
{
  SEXP l = allocVector(VECSXP, 1);
  l = PROTECT(l);
  setAttrib(l, R_ClassSymbol, mkString("a"));
  UNPROTECT(1);
  return l;
}

/* Renamed: copied into a local, and not read again. Kept. */
SEXP renamed(void)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  SEXP m = l;
  UNPROTECT(1);
  return m;
}

/* Copied into another variable, and read again: both forget the class. */
SEXP copied(void)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  SEXP m = l;
  if (LENGTH(m) > 1) Rf_error("no");
  UNPROTECT(1);
  return l;
}

/* Copied in a loop: the copy may be executed several times. */
SEXP copied_in_loop(int n)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  SEXP m = R_NilValue;
  for (int i = 0; i < n; i++) m = l;
  UNPROTECT(1);
  return m;
}

/* Stored into a list. */
SEXP stored_in_list(void)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  SEXP out = PROTECT(allocVector(VECSXP, 1));
  SET_VECTOR_ELT(out, 0, l);
  UNPROTECT(2);
  return l;
}

/* Stored into a struct. */
SEXP stored_in_struct(void)
{
  struct holder h;
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  h.value = l;
  UNPROTECT(1);
  return l;
}

/* Passed to a function of the package. */
SEXP passed(void)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  helper(l);
  UNPROTECT(1);
  return l;
}

/* Its address taken. */
SEXP addressed(void)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  SEXP *p = &l;
  UNPROTECT(1);
  return l;
}

static SEXP helper(SEXP x)
{
  return x;
}

/* ===== The summaries (typing_test --summaries): what a function may do to
   the classes and attributes of the values it is given, per parameter. ===== */

void unknown_function(SEXP x);
void sum_names(SEXP x, SEXP nms);

/* Only read: none. */
SEXP sum_reads(SEXP x) { return ScalarInteger(LENGTH(x)); }

/* Returned, through a copy and PROTECT. */
SEXP sum_returns(SEXP x) { SEXP y = PROTECT(x); UNPROTECT(1); return y; }

/* An attribute other than the class (the value set is stored: escapes). */
void sum_names(SEXP x, SEXP nms) { setAttrib(x, R_NamesSymbol, nms); }

/* The class, and an attribute not known statically (counts as the class). */
void sum_class(SEXP x) { setAttrib(x, R_ClassSymbol, mkString("a")); }
void sum_dynamic(SEXP x, SEXP sym) { setAttrib(x, sym, R_NilValue); }

/* Through a local copy, a callee's summary, a callee returning it. */
void sum_via_copy(SEXP x) { SEXP y = x; classgets(y, mkString("a")); }
void sum_via_callee(SEXP x) { sum_names(x, R_NilValue); }
void sum_via_returned(SEXP x) { SEXP y = sum_returns(x); setAttrib(y, R_DimSymbol, R_NilValue); }

/* Stored into a list or a field, given to unknown code: escapes. */
void sum_stored(SEXP x, SEXP l) { SET_VECTOR_ELT(l, 0, x); }
void sum_field(SEXP x, struct holder *h) { h->value = x; }
void sum_unknown(SEXP x) { unknown_function(x); }
void sum_pointer(SEXP x, void (*f)(SEXP)) { f(x); }

/* Mutually recursive: the fixpoint. */
void sum_even(SEXP x, int n);
void sum_odd(SEXP x, int n) { if (n == 0) classgets(x, R_NilValue); else sum_even(x, n - 1); }
void sum_even(SEXP x, int n) { if (n > 0) sum_odd(x, n - 1); }

/* ===== The summaries at the calls: a value of class "a", given to the
   functions above, is widened only as far as their summary says. ===== */

static SEXP classed(void)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  UNPROTECT(1);
  return l;
}

/* none: kept. */
SEXP after_reads(void) { SEXP l = PROTECT(classed()); sum_reads(l); UNPROTECT(1); return l; }

/* attrs: the class is kept, the other attributes are forgotten. */
SEXP after_names(void) { SEXP l = PROTECT(classed()); sum_names(l, R_NilValue); UNPROTECT(1); return l; }

/* class, escapes: forgotten. */
SEXP after_class(void) { SEXP l = PROTECT(classed()); sum_class(l); UNPROTECT(1); return l; }
SEXP after_stored(SEXP x) { SEXP l = PROTECT(classed()); sum_stored(l, x); UNPROTECT(1); return l; }

/* A callee that returns its argument: a copy (both forgotten), or a renaming
   when the argument is not read after (kept). */
SEXP after_returned_copy(void) { SEXP l = PROTECT(classed()); SEXP y = sum_returns(l); UNPROTECT(1); return l; }
SEXP after_returned_renaming(void) { SEXP l = PROTECT(classed()); SEXP y = sum_returns(l); UNPROTECT(1); return y; }
