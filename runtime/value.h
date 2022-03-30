#pragma once

#include "common.h"

#define REP(i, n) for (__typeof(n) i = 0; i < n; i++)

typedef intptr_t value;
typedef uintptr_t uvalue;
typedef uvalue hd_t;
typedef unsigned char u8;
typedef unsigned char *code_t;

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

#define pi8(p) (*(int8_t*)(p))
#define pu8(p) (*(uint8_t*)(p))
#define pi16(p) (*(int16_t*)(p))
#define pu16(p) (*(uint16_t*)(p))

#define Br16 do {if ((value)pc & 1) pc++; pc += pi16(pc);} while (0)
#define Br16if(cond) do {if ((value)pc & 1) pc++; if (cond) pc += pi16(pc); else pc += sizeof(i16);} while (0)

// value

#define Is_int(x) ((x) & 1)
#define Is_block(x) (! ((x) & 1))

/* Structure of the header:

For 32-bit architectures:

bits  31  20 19    8 7   0
     +------+-------+-----+
     | size | color | tag |
     +------+-------+-----+
      31                 0
     +--------------------+
     | xor of prev & next |
     +--------------------+

For 64-bit architectures:

bits  63  36 35    8 7   0
     +------+-------+-----+
     | size | color | tag |
     +------+-------+-----+
      63                 0
     +--------------------+
     | xor of prev & next |
     +--------------------+

*/

#define Gcsize_offset 8
#if WO