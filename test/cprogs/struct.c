#include <Rinternals.h>


struct Point {
  int x;
  int y;
};

void f(struct Point p) {
  int a = p.x;
  int b = p.y;
}

struct Point g() {
    struct Point p;
    p.x = 1;
    p.y = 2;
    return p;
}

void h(struct Point* p) {
  int a = p->x;
  int b = p->y;
}

void h2(struct Point* p) {
  int a = (*p).x;
  int b = (*p).y;
}

void i(struct Point* p) {
  p->x = 3;
  p->y = 4;
}

void i2(struct Point* p) {
  (*p).x = 3;
  (*p).y = 4;
}


// // from package clue 
typedef struct{
  int        n; 
  double   **C; 
  double   **c; 
  int       *s; 
  int       *f; 
  int       na; 
  int     runs; 
  double  cost; 
  time_t rtime; 
} AP;

// ap_free currently takes too long to type: 1min43s on a PC (2026)
// void ap_free(AP *p)
// {
//     int i;

//     free(p->s);
//     free(p->f);

//     for(i = 1; i <= p->n; i++){
//       free(p->C[i]);
//       free(p->c[i]);
//     }
    
//     free(p->C);
//     free(p->c);
//     free(p);
// }

AP *ap_create_problem(double *t, int n)
{
    int i,j;
    AP *p;
    
    p = (AP*) malloc(sizeof(AP)); 
    if(p == NULL)
	return NULL;
    
   p->n = n;

  //   p->C  = (double **) malloc((n + 1) * sizeof(double *));
  //   p->c  = (double **) malloc((n + 1) * sizeof(double *));
  //   if(p->C == NULL || p->c == NULL)
	// return NULL;
    
  //   for(i = 1; i <= n; i++){
	// p->C[i] = (double *) calloc(n + 1, sizeof(double));
	// p->c[i] = (double *) calloc(n + 1, sizeof(double));
	// if(p->C[i] == NULL || p->c[i] == NULL)
	//     return NULL;
  //   }
  
    
    for(i = 1; i <= n; i++)
	for( j = 1; j <= n; j++){
	    p->C[i][j] = t[n*(j - 1) + i - 1];
	    // p->c[i][j] = t[n*(j - 1) + i - 1];
	}
    p->cost = 0;
    p->s = NULL;
    p->f = NULL;
    return p;
}

/* Regression for an upstream sstt Tallying.Unsat that previously escaped
 * Reconstruction.refine. Minimised from utf8's utf8lite_text_iter_make:
 * the three assignments below — pointer-field copy, pointer + sibling-int,
 * sibling-int copy — together produced the unsatisfiable constraint set
 * that the local handlers in tallying.ml did not catch.
 */
struct UnsatA { int *p; int a; };
struct UnsatB { int *p; int x; };

void unsat_iter(struct UnsatB *b, struct UnsatA *a) {
    b->p = a->p;
    b->p = b->p + a->a;
    b->x = a->a;
}
/* Regression: every anonymous struct used to be named "", so one nested by
 * value in another anonymous struct was read as a recursive occurrence of
 * the outer one. The outer type became an uninhabited recursive record and
 * pointers to it c_null. Minimised from curl's `reference`, which embeds
 * `memory` and `async` this way.
 */
typedef struct { int x; } AnonInner;
typedef struct { AnonInner m; } AnonOuter;
typedef struct { int a; AnonInner m; } AnonOuter2;
typedef struct { AnonInner *m; } AnonOuterPtr;

int anon_nested(AnonOuter *o) {
  return o->m.x;
}

int anon_nested2(AnonOuter2 *o) {
  return o->a + o->m.x;
}

AnonOuter *anon_nested_cast(void *v) {
  return (AnonOuter *) v;
}

int anon_nested_ptr(AnonOuterPtr *o) {
  return o->m->x;
}

/* Struct initializers. C zero-initializes the fields an initializer does not
 * name; they keep their declared types here. A literal 0 written to a pointer
 * field is NULL, and a positional list fills the fields in order. Each used to
 * fail: the omitted fields were missing from the record, .p = 0 stored an int
 * in a pointer field, and { 0 } / { 1, 2 } were assigned as arrays. */
struct InitOpts { int a; int b; int *p; };
int init_use(const struct InitOpts *o) { return o->a + o->b; }
int init_partial(void) { struct InitOpts o = { .a = 1 }; return init_use(&o); }
int init_full(void) { struct InitOpts o = { .a = 1, .b = 2, .p = 0 }; return init_use(&o); }
int init_zero(void) { struct InitOpts o = { 0 }; return init_use(&o); }
int init_positional(void) { struct InitOpts o = { 1, 2 }; return init_use(&o); }

/* Structs are typed nominally: [struct NAME], with field types read from a
 * table. Typed as records, each access through the pointer parameter below
 * added record conjuncts to the parameter's type variable, and inference took
 * ~16s for three loops (clue's preprocess, two loop nests, timed out). */
typedef struct { int n; double **c; } Costs;
void sub_rows(Costs *p, double min) {
  int j, n = p->n;
  for (j = 1; j <= n; j++) p->c[1][j] -= min;
  for (j = 1; j <= n; j++) p->c[2][j] -= min;
  for (j = 1; j <= n; j++) p->c[3][j] -= min;
}

/* Writes keep the struct type; reads give the declared field type. */
void set_n(Costs *p, int n) { p->n = n; }
int local_write(void) { Costs k; k.n = 3; return k.n; }

/* A recursive struct needs no recursive type. */
struct lnode { struct lnode *next; int v; };
int second(struct lnode *l) { return l->next->v; }

/* Unions are typed nominally too: a member read gives its declared type. They
 * used to be typed [any], so [s->value.text] could not be projected. */
struct text { int len; };
struct str { union { struct text text; double raw; } value; int type; };
int text_len(struct str *s) { return s->value.text.len; }
typedef union { int i; double d; } num;
double num_d(num *n) { return n->d; }

/* The SEXP fields of a struct stay refinable: its type carries their record,
 * so reading [p->vec] as an integer vector requires [vec : INT] (vctrs'
 * init_int_poly_vec). Writing a SEXP field resets it to [any_sexp]. */
struct poly_vec { SEXP shelter; SEXP vec; const void* p_vec; int type; };
void init_int_poly_vec(struct poly_vec* p) { p->p_vec = (const void*) INTEGER(p->vec); }
void reset_vec(struct poly_vec* p, SEXP x) { p->vec = x; p->type = 1; }
