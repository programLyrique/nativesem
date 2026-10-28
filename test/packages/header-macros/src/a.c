#include <Rinternals.h>
#include "common.h"
#define first_string(x) CHAR(STRING_ELT(x, 0))
SEXP R_first(SEXP x) { return make_string(first_string(x)); }
