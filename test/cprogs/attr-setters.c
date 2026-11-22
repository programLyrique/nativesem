#include <Rinternals.h>

/* The setters of attributes other than setAttrib (see types/base.ty). Each one
   changes the value it is given, as setAttrib does: the variable has the new
   type after the call. */

/* classgets: as setAttrib(_, R_ClassSymbol, _), and returns the value. */
SEXP classgets_result(SEXP x)
{
  return classgets(x, mkString("myclass"));
}

SEXP classgets_after(SEXP x)
{
  classgets(x, mkString("myclass"));
  return x;
}

/* dimgets: as setAttrib(_, R_DimSymbol, _). */
SEXP dimgets_after(SEXP x, SEXP d)
{
  dimgets(x, d);
  return x;
}

/* SET_ATTRIB: the classes and attributes of the value become unknown, its
   content is kept. */
SEXP set_attrib_list(SEXP a)
{
  SEXP l = PROTECT(allocVector(VECSXP, 2));
  SET_ATTRIB(l, a);
  UNPROTECT(1);
  return l;
}

/* SET_OBJECT: likewise. */
SEXP set_object_list(void)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  SET_OBJECT(l, 1);
  UNPROTECT(1);
  return l;
}

/* DUPLICATE_ATTRIB(to, from): to keeps its content and takes from's classes. */
SEXP duplicate_attrib(SEXP from)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  SEXP c = PROTECT(allocVector(VECSXP, 1));
  setAttrib(c, R_ClassSymbol, mkString("src"));
  DUPLICATE_ATTRIB(l, c);
  UNPROTECT(2);
  return l;
}

/* copyMostAttrib(from, to): to's classes and attributes become unknown. */
SEXP copy_most_attrib(SEXP from)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  copyMostAttrib(from, l);
  UNPROTECT(1);
  return l;
}

/* CLEAR_ATTRIB: no class and no attribute left. */
SEXP clear_attrib(void)
{
  SEXP l = PROTECT(allocVector(VECSXP, 1));
  setAttrib(l, R_ClassSymbol, mkString("a"));
  CLEAR_ATTRIB(l);
  UNPROTECT(1);
  return l;
}
