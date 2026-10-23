// Regression test for C block-scoping. Before block-scope support, locals
// declared inside a nested compound statement [{ ... }] were lifted to the
// function-level namespace by the pre-pass in PAst.transform, so they
// silently shadowed any same-named global for the *entire* function body —
// including statements written before or after the inner block. When the
// shadowing local had an incompatible type, the type checker emitted a
// spurious error on the outer-scope uses.
//
// With block scope: an inner-block declaration is visible only from its
// declaration point until the closing brace; outer-scope uses of the same
// name continue to resolve to the global (or outer local) they should.

static int counter;
static double shadowed;

// Local [int counter] covers the whole function: this case already worked
// without nested block support and must remain working.
int local_shadows_global_whole_fn(int *p) {
  int counter;
  counter = *p;
  return counter;
}

// The interesting case: inner-block [double counter] shadows the [static int
// counter] only inside the braces. The pre-block write [counter = v] and the
// post-block read [return counter] must both type-check against the global
// [int] — not against the inner [double] local.
int local_shadows_global_inner_block(int v) {
  counter = v;
  {
    double counter;
    counter = 1.5;
    shadowed = counter;
  }
  return counter;
}

// Sibling blocks must not see each other's declarations. The second block
// declaring [double t] should not collide with the first block's [int t].
int sibling_blocks(void) {
  {
    int t;
    t = 1;
  }
  {
    double t;
    t = 2.5;
    shadowed = t;
  }
  return 0;
}

// Doubly-nested shadow: parameter [int x] shadows the global [int counter]'s
// neighbour, then an inner-block [double x] shadows the parameter; outside
// the inner block, [x] is the parameter again.
int doubly_nested(int x) {
  counter = x;
  {
    double x;
    x = 0.5;
    shadowed = x;
  }
  return x;
}

// A body made of one initialized declaration used to lose its block: the
// declaration landed in a bare Seq, so [x] was never declared and its
// assignment hit "Cannot assign to an immutable variable".
void lone_decl(void) {
  int x = 1;
}

void lone_decl_deref(int *p) {
  int x = *p;
}

void lone_multi_decl(void) {
  int a = 1, b = 2;
}
