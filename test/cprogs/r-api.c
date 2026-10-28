/*
To check the types of some of the R API functions
*/

#include <Rinternals.h>

void check_interrupt_fn(void *dummy) {
  R_CheckUserInterrupt();
}

int pending_interrupt(void) {
  return !(R_ToplevelExec(check_interrupt_fn, NULL));
}


SEXP build_extptr(SEXP addr, SEXP tag, SEXP prot) {
  return R_MakeExternalPtr( INTEGER(addr), tag, prot);
}
// TYPEOF accepts a value typed any_sexp (here the result of an R API call),
// while still narrowing a variable it is applied to.
int typeof_of_any_sexp(SEXP p) { return TYPEOF(R_ExternalPtrProtected(p)); }
// The tag of an external pointer is part of its type.
SEXP tag_of(SEXP p) { return R_ExternalPtrTag(p); }
SEXP first_of_tag(SEXP p) { return VECTOR_ELT(R_ExternalPtrTag(p), 0); }
SEXP retag(SEXP p, SEXP t) { R_SetExternalPtrTag(p, t); return R_ExternalPtrTag(p); }
int typeof_of_car(SEXP l) { if (TYPEOF(l) != LANGSXP) return 0; return TYPEOF(CAR(l)); }

// Rf_xlength / XLENGTH / LENGTH accept lists: the list branch is kept.
R_xlen_t xlength_of_list(SEXP l) { if (TYPEOF(l) != VECSXP) return 0; return Rf_xlength(l); }
SEXP last_list_elt(SEXP l) { if (TYPEOF(l) != VECSXP) return R_NilValue; R_xlen_t n = XLENGTH(l); return VECTOR_ELT(l, n - 1); }

// Rf_eval returns a SEXP, usable as one.
int typeof_of_eval(SEXP e, SEXP rho) { SEXP v = Rf_eval(e, rho); return TYPEOF(v); }

// Variable lookup and routine registration are declared.
SEXP lookup(SEXP sym, SEXP rho) { return Rf_findVar(sym, rho); }
void R_init_rapi(DllInfo *dll) { R_registerRoutines(dll, NULL, NULL, NULL, NULL); R_useDynamicSymbols(dll, FALSE); }
