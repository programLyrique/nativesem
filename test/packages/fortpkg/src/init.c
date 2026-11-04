#include <R.h>
#include <R_ext/Rdynload.h>

static R_NativePrimitiveArgType scale_t[] = {REALSXP, INTSXP, REALSXP, LGLSXP};
static R_NativePrimitiveArgType cmplx_t[] = {CPLXSXP};

/* Registered with types: its type comes from them, not from its prototype,
   whose [int *ok] would also take an integer vector. */
void F77_SUB(scale)(double *x, int *n, double *a, int *ok);

/* [count] is registered without types: its type comes from its prototype. */
void F77_SUB(count)(double *x, int *n, int *k);

static const R_FortranMethodDef fortranMethods[] = {
    {"scale", (DL_FUNC) &F77_SUB(scale), 4, scale_t},
    {"count", (DL_FUNC) &F77_SUB(count), 3},
    {"cmplx", (DL_FUNC) &F77_SUB(cmplx), 1, cmplx_t},
    {NULL, NULL, 0}};

void R_init_fortpkg(DllInfo *info)
{
    R_registerRoutines(info, NULL, NULL, fortranMethods, NULL);
    R_useDynamicSymbols(info, FALSE);
}
