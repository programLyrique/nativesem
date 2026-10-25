#include <Rinternals.h>

/* Increments are assignments: [i++] used to compute i + 1 and drop it. */
int post_inc(void) { int i = 0; i++; return i; }
int post_value(void) { int i = 0; int j = i++; return j; }
int pre_value(void) { int i = 0; int j = ++i; return j; }

/* An assignment has the value assigned. */
int chain(void) { int a, b; a = b = 3; return a; }
int assign_cond(int (*f)(void)) { int err; if ((err = f())) return err; return 0; }

/* Constants are not propagated past a branch or into a loop: the code used to
   be rewritten with the value of one path ([x + 0] became 2, [i + 2] 2). */
int after_if(int c) { int x = 1; if (c) x = 2; return x + 0; }
int else_branch(int c) { int x = 1; if (c) { x = 2; } else { x = x + 10; } return x; }
int loop_count(int n) { int i = 0; while (i < n) i = i + 2; return i; }

/* A static local holds, on entry, whatever a previous call left. */
int counter(void) { static int n = 0; n++; return n; }

/* The body of a switch is one block, and C does nothing for the values no
   case matches when there is no default. */
int switch_scope(int k) {
  int r = 0;
  switch (k) { case 1: r = 1; int t; t = 4; r = t; break; case 2: t = 6; r = t; break; }
  return r;
}
