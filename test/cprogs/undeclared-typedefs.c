#include <Rinternals.h>

/* Typedefs from headers that are not parsed (here R_xlen_t, from
 * Rinternals.h) take the type given by a [typedef] line of the .ty files.
 * They used to be [any] (rlang's r_ssize). */
typedef R_xlen_t r_ssize;
int need_int(int n);
int use_ssize(r_ssize n) { return need_int(n); }
int low_bits(uint_fast8_t c) { return c & 0x3F; }
