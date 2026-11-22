#include <Rinternals.h>

/* Class attribute is specialised to v(chr). */
SEXP first_class(SEXP x)
{
  return STRING_ELT(getAttrib(x, R_ClassSymbol), 0);
}

int n_classes(SEXP x)
{
  return LENGTH(getAttrib(x, R_ClassSymbol));
}

/* names on a bare value of unknown shape: a names attribute is always a
   character vector or NULL -> chr | NULL (see named_list_names below for the
   precise list-labels readback). */
SEXP names(SEXP x)
{
  return getAttrib(x, R_NamesSymbol);
}

/* dim is a recognized special attribute: its value is an integer vector, so
   getAttrib types as an int vector | NULL (R's dim is INTSXP, or unset). */
SEXP dim(SEXP x)
{
  return getAttrib(x, R_DimSymbol);
}

/* levels: factor levels are a character vector -> chr | NULL. */
SEXP levels(SEXP x)
{
  return getAttrib(x, R_LevelsSymbol);
}

/* dimnames: a list (VECSXP) -> list | NULL. */
SEXP dimnames(SEXP x)
{
  return getAttrib(x, R_DimNamesSymbol);
}

/* row.names: an integer or character vector -> (int | chr) | NULL. */
SEXP row_names(SEXP x)
{
  return getAttrib(x, R_RowNamesSymbol);
}

/* setAttrib(_, R_ClassSymbol, _): the result keeps the value's content and
   carries the class given by the third argument. When that argument's type
   exposes concrete class-name singletons (here mkString("myclass") :
   v[1](chr("myclass"))) the class is set to exactly those names. */
SEXP set_class(SEXP x)
{
  return setAttrib(x, R_ClassSymbol, mkString("myclass"));
}

/* The value's content is preserved: tagging a VECSXP keeps the list shape. */
SEXP set_class_on_list(void)
{
  SEXP l = allocVector(VECSXP, 2);
  return setAttrib(l, R_ClassSymbol, mkString("myclass"));
}

/* When the class value's names are not statically known, the class falls back
   to "any class" (attr_any). */
SEXP set_class_unknown(SEXP x, SEXP cls)
{
  return setAttrib(x, R_ClassSymbol, cls);
}

/* Build-up idiom: a multi-element class vector assembled with SET_STRING_ELT.
   allocVector(STRSXP,2) seeds "" (R's R_BlankString default) and each
   SET_STRING_ELT unions its string into the element type, so cls types as
   v[2]("" | "data.frame" | "tbl_df"). The class reader drops the "" allocation
   default, so setAttrib recovers exactly <data.frame, tbl_df>. */
SEXP set_class_buildup(SEXP x)
{
  SEXP cls = allocVector(STRSXP, 2);
  SET_STRING_ELT(cls, 0, mkChar("tbl_df"));
  SET_STRING_ELT(cls, 1, mkChar("data.frame"));
  return setAttrib(x, R_ClassSymbol, cls);
}

/* Partial build-up: slot 1 left unset, so cls soundly types as v[2]("" | "a")
   (slot 1 really is ""). The "" allocation default is still dropped from the
   recovered class, leaving <a>. */
SEXP set_class_buildup_partial(SEXP x)
{
  SEXP cls = allocVector(STRSXP, 2);
  SET_STRING_ELT(cls, 0, mkChar("a"));
  return setAttrib(x, R_ClassSymbol, cls);
}

/* Refined branch: setAttrib installs a concrete class and getAttrib reads it
   back, narrowing to v[N](class-name singletons). LENGTH yields the class count
   and STRING_ELT preserves the singleton through its 'a parameter. */
SEXP refined_class(SEXP x)
{
  SEXP tagged = setAttrib(x, R_ClassSymbol, mkString("myclass"));
  return getAttrib(tagged, R_ClassSymbol);
}
int refined_class_length(SEXP x)
{
  SEXP tagged = setAttrib(x, R_ClassSymbol, mkString("myclass"));
  return LENGTH(getAttrib(tagged, R_ClassSymbol));
}
SEXP refined_class_first(SEXP x)
{
  SEXP tagged = setAttrib(x, R_ClassSymbol, mkString("myclass"));
  return STRING_ELT(getAttrib(tagged, R_ClassSymbol), 0);
}

/* ===== Arbitrary (non-class) attributes, stored in the attrs field ===== */

/* A non-class symbol (R_DimSymbol -> "dim") set to a *constructed* value: the
   attribute is definitely present, so getAttrib reads back exactly that value
   (a length-2 int vector), with no spurious NULL. */
SEXP dim_roundtrip_constructed(SEXP x)
{
  SEXP d = allocVector(INTSXP, 2);
  SEXP tagged = setAttrib(x, R_DimSymbol, d);
  return getAttrib(tagged, R_DimSymbol);
}

/* A user attribute named via install("..."), set to a constructed string: the
   literal name "label" is recovered at the call site and the value reads back
   as the concrete string "hello". */
SEXP install_roundtrip_constructed(SEXP x)
{
  SEXP tagged = setAttrib(x, install("label"), mkString("hello"));
  return getAttrib(tagged, install("label"));
}

/* The set value also flows from an argument (not just constructed): getAttrib
   recovers the argument's type. */
SEXP dim_roundtrip_arg(SEXP x, SEXP d)
{
  SEXP tagged = setAttrib(x, R_DimSymbol, d);
  return getAttrib(tagged, R_DimSymbol);
}

/* Class and a non-class attribute coexist: set the class, then a constructed
   dim attribute; the class is still readable (the named-attr setAttrib
   preserves classes). */
SEXP class_then_dim_keeps_class(SEXP x)
{
  SEXP tagged = setAttrib(x, R_ClassSymbol, mkString("myclass"));
  tagged = setAttrib(tagged, R_DimSymbol, allocVector(INTSXP, 2));
  return getAttrib(tagged, R_ClassSymbol);
}

/* getAttrib of a special attribute never set on an arbitrary value: bounded by
   the attribute's R type rather than any_sexp -- here an integer vector | NULL
   for dim (it may carry a dim, or not). */
SEXP get_unset_attr(SEXP x)
{
  return getAttrib(x, R_DimSymbol);
}

/* getAttrib with a dynamic (statically-unknown) symbol: any SEXP or NULL. */
SEXP get_dynamic_attr(SEXP x, SEXP sym)
{
  return getAttrib(x, sym);
}

/* setAttrib mutates its first argument in place. Here the result is discarded
   and the *parameter* is returned, so the new attribute must still show on the
   return type -- this relies on setAttrib being rewritten to [x = setAttrib(x,
   ...)] (see PAst.process_call), like SET_VECTOR_ELT/SET_STRING_ELT. */
SEXP set_attr_on_param_return_param(SEXP x)
{
  setAttrib(x, R_DimSymbol, allocVector(INTSXP, 2));
  return x;
}

/* Same, for the class attribute: the returned parameter carries class <myclass>. */
SEXP set_class_on_param_return_param(SEXP x)
{
  setAttrib(x, R_ClassSymbol, mkString("myclass"));
  return x;
}

/* Build a factor from scratch: an integer vector with class "factor" and a
   levels attribute (a character vector of level names). The result carries both
   -- content int3, class <factor>, and levels holding the built strings. (The
   "" in the levels is the STRSXP allocation default, as in set_class_buildup;
   the class reader drops it for classes but a plain attribute keeps it.) */
SEXP make_factor(void)
{
  SEXP x = allocVector(INTSXP, 3);
  SEXP lvls = allocVector(STRSXP, 2);
  SET_STRING_ELT(lvls, 0, mkChar("a"));
  SET_STRING_ELT(lvls, 1, mkChar("b"));
  setAttrib(x, R_LevelsSymbol, lvls);
  setAttrib(x, R_ClassSymbol, mkString("factor"));
  return x;
}

/* Read the levels back off a freshly built factor: the concrete level strings. */
SEXP factor_levels(void)
{
  SEXP x = allocVector(INTSXP, 3);
  SEXP lvls = allocVector(STRSXP, 2);
  SET_STRING_ELT(lvls, 0, mkChar("a"));
  SET_STRING_ELT(lvls, 1, mkChar("b"));
  setAttrib(x, R_LevelsSymbol, lvls);
  setAttrib(x, R_ClassSymbol, mkString("factor"));
  return getAttrib(x, R_LevelsSymbol);
}

/* getAttrib(_, R_NamesSymbol) on a named list reads the field labels back as a
   names vector: exactly the names, as clean string singletons (no "" build-up
   artifact, since they come from the list labels rather than a STRSXP read). */
SEXP named_list_names(void)
{
  const char *nms[] = {"a", "b", ""};
  SEXP l = mkNamed(VECSXP, nms);
  return getAttrib(l, R_NamesSymbol);
}

/* A positional (unnamed) list has only placeholder labels (_0, _1, ...), so it
   has no R names -> getAttrib(names) is NULL. */
SEXP unnamed_list_names(void)
{
  SEXP l = allocVector(VECSXP, 2);
  return getAttrib(l, R_NamesSymbol);
}

/* setAttrib(list, R_NamesSymbol, nms): the names STRSXP is built statically, so
   const-prop tracks its ordered contents ["a","b"] and the list's positional
   slots are relabeled -> { a: ...; b: ... }. (This is what makes setAttrib-names
   precise: the type-level STRSXP element union is unordered, but const-prop
   preserves slot order.) */
SEXP set_names_on_list(void)
{
  SEXP l = allocVector(VECSXP, 2);
  SEXP nms = allocVector(STRSXP, 2);
  SET_STRING_ELT(nms, 0, mkChar("a"));
  SET_STRING_ELT(nms, 1, mkChar("b"));
  setAttrib(l, R_NamesSymbol, nms);
  return l;
}

/* Reading the names back off the relabeled list: exactly c("a","b"), no "". */
SEXP set_names_then_get(void)
{
  SEXP l = allocVector(VECSXP, 2);
  SEXP nms = allocVector(STRSXP, 2);
  SET_STRING_ELT(nms, 0, mkChar("a"));
  SET_STRING_ELT(nms, 1, mkChar("b"));
  setAttrib(l, R_NamesSymbol, nms);
  return getAttrib(l, R_NamesSymbol);
}

/* ===== Constructors (see types/base.ty): a fresh value has no class and no
   attribute, <> with {}, until a setter gives it some or it escapes (cf.
   escapes.c). ===== */
SEXP scalar_real(void) { return ScalarReal(1.5); }

SEXP scalar_integer(void) { return ScalarInteger(3); }

SEXP scalar_logical(void) { return ScalarLogical(1); }

SEXP make_string(void) { return mkString("a"); }

SEXP scalar_string(void) { return ScalarString(mkChar("b")); }

SEXP alloc_vector(int n) { return allocVector(REALSXP, n); }

/* mkNamed: no class, and the names. */
SEXP make_named_int(void)
{
  const char *nms[] = {"x", "y", ""};
  return mkNamed(INTSXP, nms);
}

/* A setter adds an attribute to a fresh value: only that one. */
SEXP with_dim(void)
{
  SEXP x = PROTECT(allocVector(INTSXP, 4));
  SEXP d = PROTECT(allocVector(INTSXP, 2));
  INTEGER(d)[0] = 2;
  INTEGER(d)[1] = 2;
  setAttrib(x, R_DimSymbol, d);
  UNPROTECT(2);
  return x;
}
