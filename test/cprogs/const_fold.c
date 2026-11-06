#define BIT0 (1 << 0)
#define BIT1 (1 << 1)
#define MASK_ALL (BIT0 | BIT1)
#define VERSION_CODE ((1 << 16) | (2 << 8) | 3)
#define HALF (1.0 / 2.0)
#define MIX  ((double)1 / 2)
#define NEG_FLAG (-(1 << 3))

enum Flags {
  F_A = BIT0,
  F_B = BIT1,
  F_AB = F_A | F_B,
  F_NEXT,
  F_SHIFTED = 1 << 4,
  F_VERSION = VERSION_CODE
};

int use_bit(void) { return BIT0; }
int use_mask(void) { return MASK_ALL; }
int use_version(void) { return VERSION_CODE; }
double use_half(void) { return HALF; }
int use_f_ab(void) { return F_AB; }
int use_f_shifted(void) { return F_SHIFTED; }
int use_f_version(void) { return F_VERSION; }

// In a hexadecimal literal, e and E are digits: these are integers.
#define HEX_e 0x9e3779b9
#define HEX_E 0xE5
unsigned int use_hex_e(void) { return HEX_e; }
unsigned int use_hex_E(void) { return HEX_E; }
