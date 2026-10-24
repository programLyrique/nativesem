#include <Rinternals.h>

/* A function-like macro and a function of the same name are two entities:
   [clamp(x)] expands the macro, [(clamp)(x, hi)] calls the function. */
int clamp(int x, int hi);
#define clamp(x) (clamp)(x, 10)

int (clamp)(int x, int hi) { return x > hi ? hi : x; }

/* Through the macro: one argument, the default supplied. */
int via_macro(int x) { return clamp(x); }

/* The function itself: two arguments. */
int via_function(int x) { return (clamp)(x, 3); }

/* After #undef, clamp(...) calls the function again. */
#undef clamp
int after_undef(int x) { return clamp(x, 5); }
