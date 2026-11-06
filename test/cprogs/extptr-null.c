#include <Rinternals.h>

// External pointers whose address may be null. Dereferencing a null pointer
// is undefined behaviour, typed as an error: *c_null is empty.

// A check splits off the null pointer, externalptr(empty).
SEXP check_eq(SEXP ptr) {
  if (R_ExternalPtrAddr(ptr) == NULL) Rf_error("dead");
  return R_NilValue;
}
SEXP check_not(SEXP ptr) {
  void *s = R_ExternalPtrAddr(ptr);
  if (!s) Rf_error("dead");
  return R_NilValue;
}

// Without a check, the dereference does it.
SEXP unchecked(SEXP ptr) {
  int *q = R_ExternalPtrAddr(ptr);
  return Rf_ScalarInteger(*q);
}

static SEXP call_check(SEXP ptr) {
  if (R_ExternalPtrAddr(ptr) == NULL) Rf_error("dead");
  return R_NilValue;
}
SEXP call_null(SEXP x) {
  return call_check(R_MakeExternalPtr(NULL, R_NilValue, R_NilValue));
}
SEXP call_int(SEXP x) {
  return call_check(R_MakeExternalPtr(INTEGER(x), R_NilValue, R_NilValue));
}

// The target of an external pointer may be a C string, and a pointer that is
// not null stays so.
SEXP make_str(SEXP x) {
  char *s = "abc";
  return R_MakeExternalPtr(s, R_NilValue, R_NilValue);
}
SEXP roundtrip_str(SEXP x) {
  char *s = "abc";
  SEXP p = R_MakeExternalPtr(s, R_NilValue, R_NilValue);
  char *t = R_ExternalPtrAddr(p);
  return Rf_mkString(t);
}
SEXP roundtrip_int(SEXP x) {
  SEXP p = R_MakeExternalPtr(INTEGER(x), R_NilValue, R_NilValue);
  int *q = R_ExternalPtrAddr(p);
  return Rf_ScalarInteger(*q);
}
