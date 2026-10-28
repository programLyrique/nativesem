enum Color
{
    RED,
    GREEN = 4,
    BLUE,
    CYAN = BLUE + 2
};

typedef enum Direction
{
    DIR_LEFT = -1,
    DIR_NONE = 0,
    DIR_RIGHT = 1
} Direction;

enum Mask
{
    MASK_READ = 1,
    MASK_WRITE = 1 << 1,
    MASK_RW = MASK_READ | MASK_WRITE
};

enum Color keep_color(enum Color color)
{
    if (color == RED)
        return GREEN;
    switch (color)
    {
    case BLUE:
        return GREEN;
    default:
        return RED;
    }
}

Direction flip_direction(Direction dir)
{
    if (dir == DIR_LEFT)
        return DIR_RIGHT;
    if (dir == DIR_RIGHT)
        return DIR_LEFT;
    return DIR_NONE;
}

int mask_value()
{
    return MASK_READ;
}

/* An enum whose values form many intervals is typed by their hull (curl's
 * CURLoption); its enumerators keep their exact values. */
typedef enum { A0 = 0, A1, A2, A3 } small_t;
typedef enum { B0 = 0, B1 = 10, B2 = 20, B3 = 30, B4 = 40, B5 = 50, B6 = 60, B7 = 70, B8 = 80, B9 = 90 } sparse_t;
int use_small(small_t s) { return s == A2; }
int use_sparse(sparse_t s) { if (s == B3) return 1; return 0; }
sparse_t give(void) { return B5; }
