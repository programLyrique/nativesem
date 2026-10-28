#include <Rinternals.h>

#define FEATURE_FLAG

#ifdef FEATURE_FLAG
SEXP selected_by_ifdef(SEXP x) {
    return x;
}
#else
SEXP skipped_ifdef(SEXP x) {
    return R_NilValue;
}
#endif

#ifndef MISSING_FLAG
SEXP selected_by_ifndef(SEXP x) {
    return x;
}
#else
SEXP skipped_ifndef(SEXP x) {
    return R_NilValue;
}
#endif

#ifdef __linux__
SEXP selected_by_linux_define(SEXP x) {
    return x;
}
#elifndef __linux__
SEXP skipped_elifndef(SEXP x) {
    return R_NilValue;
}
#else
SEXP skipped_linux_else(SEXP x) {
    return R_NilValue;
}
#endif

/* The body of an [if] is the first statement of the selected branch. */
void body_in_branch(int err) {
    if (err != 0)
#ifdef MISSING_FLAG
        Rf_error("a");
#else
        Rf_error("b");
#endif
}

int else_body_in_branch(int err) {
    if (err != 0)
        return 1;
    else
#ifndef MISSING_FLAG
        return 2;
#endif
}

/* An empty branch: the body is the statement after [#endif]. */
int body_after_branch(int err) {
    if (err != 0)
#ifdef MISSING_FLAG
        return 1;
#endif
    return 2;
    return 3;
}
