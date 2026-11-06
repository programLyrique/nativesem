#include <Rinternals.h>

SEXP r_plus_scalar_real(SEXP a, SEXP b)
{
    // real only
    SEXP result = PROTECT(allocVector(REALSXP, 1));
    REAL(result)
    [0] = REAL(a)[0] + REAL(b)[0];
    UNPROTECT(1);
    return result;
}

SEXP make_int_or_float(int test)
{
    SEXP result;
    if (test)
    {
        result = PROTECT(allocVector(INTSXP, 1));
        INTEGER(result)
        [0] = 42;
        UNPROTECT(1);
        return result;
    }
    else
    {
        result = PROTECT(allocVector(REALSXP, 2));
        REAL(result)
        [0] = 42.0;
        UNPROTECT(1);
        return result;
    }
}

SEXP incr_real(SEXP a)
{
    if (!isReal(a) || LENGTH(a) != 1)
    {
        fail("Input must be a single real"); // Ty.empty -> Ty.any
    }
    double val = REAL(a)[0];                        // real[1] -> C_double
    val = val + 1.0;                                // (C_double,C_double) -> C_double
    SEXP result = PROTECT(allocVector(REALSXP, 1)); // REALSXP * 1 -> real[1]
    REAL(result)
    [0] = val;
    UNPROTECT(1);
    return result;
}

int test_int_or_real(SEXP a)
{
    if (isInteger(a))
    {
        return 1;
    }
    if (isReal(a))
    {
        return 0;
    }
}

int test_isInt(SEXP a)
{
    if (isInteger(a))
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

int test_isInt2(SEXP a)
{
    return isInteger(a) ? 1 : 0;
}

SEXP r_plus_scalar_strict(SEXP a, SEXP b)
{
    // int and real
    if (isInteger(a) && isInteger(b))
    {
        SEXP result = PROTECT(allocVector(INTSXP, 1));
        INTEGER(result)
        [0] = INTEGER(a)[0] + INTEGER(b)[0];
        UNPROTECT(1);
        return result;
    }
    else if (isReal(a) && isReal(b))
    {
        SEXP result = PROTECT(allocVector(REALSXP, 1));
        REAL(result)
        [0] = REAL(a)[0] + REAL(b)[0];
        UNPROTECT(1);
        return result;
    }
    else
    { // removing that will add one overload for plus
        fail("Inputs must be both integer or both real");
    }
}

SEXP r_plus_scalar(SEXP a, SEXP b)
{
    // int and real
    if (isInteger(a) && isInteger(b))
    {
        SEXP result = PROTECT(allocVector(INTSXP, 1));
        INTEGER(result)
        [0] = INTEGER(a)[0] + INTEGER(b)[0];
        UNPROTECT(1);
        return result;
    }
    else if (isReal(a) || isReal(b))
    {
        double va = isInteger(a) ? INTEGER(a)[0] : REAL(a)[0];
        double vb = isInteger(b) ? INTEGER(b)[0] : REAL(b)[0];
        SEXP result = PROTECT(allocVector(REALSXP, 1));
        REAL(result)
        [0] = va + vb;
        UNPROTECT(1);
        return result;
    }
    else
    { // removing that will add one overload for plus
        fail("Inputs must be both integer or both real");
    }
}

SEXP r_plus_all(SEXP a, SEXP b)
{
    // handle both ints and reals, whatever their sizes
    // recycle the shorter vector
    if (isInteger(a) && isInteger(b))
    {
        // both are integer vectors
        R_xlen_t na = XLENGTH(a);
        R_xlen_t nb = XLENGTH(b);
        R_xlen_t nres;
        if (na > nb)
        {
            nres = na;
        }
        else
        {
            nres = nb;
        }
        SEXP result = PROTECT(allocVector(INTSXP, nres));
        for (R_xlen_t i = 0; i < nres; i++)
        {
            INTEGER(result)
            [i] = INTEGER(a)[i % na] + INTEGER(b)[i % nb];
        }
        UNPROTECT(1);
        return result;
    }
    else
    {
        // at least one is a real vector
        R_xlen_t na = XLENGTH(a);
        R_xlen_t nb = XLENGTH(b);
        R_xlen_t nres;
        if (na > nb)
        {
            nres = na;
        }
        else
        {
            nres = nb;
        }
        SEXP result = PROTECT(allocVector(REALSXP, nres));
        for (R_xlen_t i = 0; i < nres; i++)
        {
            double va = isInteger(a) ? INTEGER(a)[i % na] : REAL(a)[i % na];
            double vb = isInteger(b) ? INTEGER(b)[i % nb] : REAL(b)[i % nb];
            REAL(result)
            [i] = va + vb;
        }
        UNPROTECT(1);
        return result;
    }
}

// The type could be simplified by merging the 3 branches
static R_INLINE int R_integer_plus(int x, int y, bool *pnaflag)
{
    if (x == NA_INTEGER || y == NA_INTEGER)
        return NA_INTEGER;

    if (((y > 0) && (x > (R_INT_MAX - y))) ||
        ((y < 0) && (x < (R_INT_MIN - y))))
    {
        if (pnaflag != NULL)
            *pnaflag = true;
        return NA_INTEGER;
    }
    return x + y;
}

// The bitwise operators take any int, NA_INTEGER included.
int bit_xor(int a, int b) { return a ^ b; }
