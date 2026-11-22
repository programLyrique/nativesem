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

/* Copied into another variable: both forget the class. */
SEXP copied(void)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  SEXP m = l;
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
