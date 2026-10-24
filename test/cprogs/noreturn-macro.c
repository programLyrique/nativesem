#include <Rinternals.h>

/* rlang's error helper (vendored in vctrs and others): a noreturn function
   pointer, wrapped by a variadic macro of the same name that calls it as
   [(r_stop_internal)(...)]. */
extern
r_no_return
void (*r_stop_internal)(const char* file,
                        int line,
                        SEXP call,
                        const char* fmt,
                        ...);

SEXP r_peek_frame(void);

#define r_stop_internal(...)                            \
  (r_stop_internal)(__FILE__, __LINE__, r_peek_frame(), \
                    __VA_ARGS__)

#define r_stop_unreachable()                            \
  (r_stop_internal)(__FILE__, __LINE__, r_peek_frame(), \
                    "Reached the unreachable")

typedef r_no_return void (*r_stop_internal_t)(const char*, int, SEXP, const char*, ...);

/* Assigning the variable is still legal: the macro does not shadow it.
   As in rlang, the value is cast through a noreturn typedef. */
void init_stop_internal(void) {
  r_stop_internal = (r_stop_internal_t) R_GetCCallable("rlang", "rlang_stop_internal2");
}

/* The error path is typed [empty]: r_stop_internal never returns. */
int stop_after_check(int x) {
  if (x > 0) return x;
  r_stop_internal("Unexpected `%d`.", x);
}

int stop_unreachable(int x) {
  switch (x) {
  case 0: return 1;
  case 1: return 2;
  default: r_stop_unreachable();
  }
}

/* _Noreturn and __attribute__((noreturn)) prototypes. */
_Noreturn void die(const char* msg);
void die2(const char* msg) __attribute__((noreturn));
__attribute__((noreturn)) void die3(const char* msg);

int die_after_check(int x) {
  if (x > 0) return x;
  die("negative");
}

/* A variadic C function with a body: extra arguments are accepted. */
int count_fmt(const char* fmt, ...) { return 1; }
int call_count_fmt(void) { return count_fmt("%d %d", 1, 2); }
