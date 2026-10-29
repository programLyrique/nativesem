#include <Rinternals.h>

/* Implicit C [void* -> T*] conversion at initialization.
   [malloc] returns the top pointer [c_ptr]; storing it into a concrete
   [char*] must narrow it to [char*], otherwise subscript writes and the
   char*-typed string API ([mkCharCE]) reject it.
   Before the fix, [s] stayed [c_ptr] and this was an untypeable application. */
SEXP C_malloc_to_charptr(int n) {
  char *s = malloc(n + 3);
  s[0] = '[';
  s[1] = ']';
  s[2] = '\0';
  return mkCharCE(s, 0);
}

/* A [void*] declaration must NOT clobber a more precise initializer: the
   conversion only fires for concrete (non-[void]) pointer targets. Here the
   initializer is already a [char*] (c_string), so [p] keeps it and the
   char*-typed use type-checks without an explicit cast. */
extern char *get_buf(void);
SEXP C_voidptr_keeps_precision(void) {
  void *p = get_buf();
  return mkCharCE(p, 0);
}

/* A pointer with no common pointee with the declared type -- here a struct
   laid out in a raw vector, whose RAW() buffer is typed [*c_int] -- gets the
   declared type, as an explicit cast would (with a warning on stderr). The
   intersection used to give it the type of NULL (the pointer to [empty]),
   and the code reading through it was unreachable: the function was typed
   [-> empty] (vctrs' new_poly_vec, hence its dictionaries). */
struct box { SEXP vec; int type; };

void *raw_begin(SEXP x) { return RAW(x); }

/* Through a [void*] helper. */
SEXP C_raw_struct_via_helper(SEXP x) {
  SEXP self = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct box)));
  struct box *b = raw_begin(self);
  b->vec = x;
  UNPROTECT(1);
  return x;
}

/* Through a [void*] local. */
SEXP C_raw_struct_via_void(SEXP x) {
  SEXP self = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct box)));
  void *p = RAW(self);
  struct box *b = p;
  b->vec = x;
  UNPROTECT(1);
  return x;
}

/* With an explicit cast: this was always typed. */
SEXP C_raw_struct_via_cast(SEXP x) {
  SEXP self = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct box)));
  struct box *b = (struct box *) RAW(self);
  b->vec = x;
  UNPROTECT(1);
  return x;
}

/* A pointer with a common pointee is still narrowed, and NULL stays NULL. */
double *C_maybe_null(SEXP x, int k) {
  double *p = k ? REAL(x) : NULL;
  return p;
}
