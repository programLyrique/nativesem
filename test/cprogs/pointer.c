// Pointers (and arrays as arrays are currently modeled as pointers)

void assign(double *t) {
    t[0] = 42;
}


void assign_in_array(double **p) {
    p[0][0] = 42;
}

struct S {
    double ** C;
    int n;
};

void assign_in_struct(struct S *p) {
    p->C[0][0] = 42;
}

struct S *assign_n(int n) {
    struct S *s;
    s = (struct S*) malloc(sizeof(struct S));
    s->n = n;
    return s;
}

// Writes through a bare pointer dereference.

int *star_return(int *p, int v) {
    *p = v;
    return p;
}

int star_then_read(int *p, int v) {
    *p = v;
    return *p;
}

void star_void(int *p, int v) {
    *p = v;
}

void star_two_writes(int *p, int v1, int v2) {
    *p = v1;
    *p = v2;
}
/* C converts void* to T* on assignment too, not only at an initialization
 * (clue's ap_hungarian: [ri = calloc(...)] then [cover(p, ri, ci)]). */
int first(int *xs) { return xs[0]; }
int assign_alloc(int n) {
  int *xs;
  xs = calloc(n, sizeof(int));
  return first(xs);
}
