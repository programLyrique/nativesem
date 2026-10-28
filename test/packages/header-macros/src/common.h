/* Function-like macros of a package are bound in package mode too (curl's
   [make_string] in curl-common.h). */
#define make_string(x) x != NULL ? Rf_mkString(x) : Rf_ScalarString(NA_STRING)
