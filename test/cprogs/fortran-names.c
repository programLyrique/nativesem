// R's F77_SUB, F77_NAME and F77_CALL name the Fortran symbol [x_].
#include <R.h>

void F77_SUB(linbin)(double *x, int *n, double *gcounts);
void F77_NAME(rlbin)(double *x, int *n);
extern void F77_CALL(scale)(double *x, int *n, double *a);

void scale_twice(double *x, int *n, double *a)
{
    F77_CALL(scale)(x, n, a);
    F77_CALL(scale)(x, n, a);
}
