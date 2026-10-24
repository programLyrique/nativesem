#include <stdbool.h>

/* A bool parameter accepts any int: C converts the argument. */
bool takes_bool(bool b) { return !b; }
bool int_to_bool(int i) { return takes_bool(i); }

int takes_int(int i) { return i + 1; }
int bool_to_int(bool b) { return takes_int(b); }

/* && and || take any scalar operand, pointers included. */
int and_int(int a, int b) { return a && b; }
int and_ptr(int a, int *p) { return a && p; }
int or_ptr(int *p, int *q) { return p || q; }

/* Short circuit: *p is only evaluated when p is not NULL. */
int guarded(int *p) { if (p && *p > 0) return *p; return 0; }
int guarded_or(int *p) { if (!p || *p == 0) return 0; return *p; }
