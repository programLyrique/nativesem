#include <Rinternals.h>


RBoolean isInteger2(SEXP a) {
    switch (TYPEOF(a))
    {
    case INTSXP:
        return TRUE;
        break;

    default:
        return FALSE;
        break;
    }
 }

RBoolean isInteger3(SEXP a) {
    if(TYPEOF(a) == INTSXP) {
        return TRUE;
    } else {
        return FALSE;
    }
}

RBoolean checkList(SEXP s) {
    if(TYPEOF(s) == VECSXP ) {
        return TRUE;
    } else {
        return FALSE;
    }
}

INLINE_FUN Rboolean isVectorList(SEXP s)
{
    switch (TYPEOF(s)) {
    case VECSXP:
    case EXPRSXP:// currently encoded as {;lang|sym}
	return TRUE;
    default:
	return FALSE;
    }
}


INLINE_FUN R_len_t length2(SEXP s)
{
    switch (TYPEOF(s)) {
    case NILSXP:
	return 0;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case CHARSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
	return LENGTH(s);
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    {
	int i = 0;
	while (s != NULL && s != R_NilValue) {
	    i++;
	    s = CDR(s);
	}
	return i;
    }
    case ENVSXP:
	return Rf_envlength(s);
    default:
	return 1;
    }
}



// The switch above, but written with an if
INLINE_FUN R_len_t length3(SEXP s)
{
    int type = TYPEOF(s);
    if (type == NILSXP) {
        return 0;
    } else if (type == LGLSXP || type == INTSXP || type == REALSXP || type == CPLXSXP || type == STRSXP || type == RAWSXP) {
        return LENGTH(s);
    } else {
        return 1;
    }
}

// A case label inside an #if: tree-sitter ends the case at the #endif, and
// the statements after it still belong to the cases above (utf8's
// rutf8_encodes_utf8).
enum pp_case { PP_A, PP_B, PP_C, PP_D };
int case_in_if(enum pp_case ce)
{
    switch (ce) {
    case PP_A:
#if !defined(_WIN32)
    case PP_C:
#endif
        return 1;
    default:
        return 0;
    }
}
