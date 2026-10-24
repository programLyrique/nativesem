/* A [static] defined in a header, used by the .c file that includes it:
   local includes are not followed, so it must be bound package-wide. */
static const int stage1[] = { 3, 1, 4, 1, 5 };
static int counter = 0;
static int* const counter_ptr = &counter;
