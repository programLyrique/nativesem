#include <Rinternals.h>
#include "private/tbl.h"
int lookup(int i) { *counter_ptr += 1; return stage1[i]; }
SEXP R_lookup(SEXP i) { return Rf_ScalarInteger(lookup(INTEGER(i)[0])); }
