#include "common.h"
#include "instruct.h"
#include "value.h"
#include "prim.h"
#include "error.h"
#include "str.h"

#define DEBUG

#ifdef DEBUG
# undef DIRECT_JUMP
#endif

//#define DIRECT_JUMP
#define Arg_stack_size 16384
#define Ret_stack_size 16384
#define Trap_stack_size 16384

#define DIVISION_BY_ZERO_EXN 1 // ld.ml division_by_zero_tag

bool trace = false;
bool verbose = false;
value global_value;
hd_t first_atoms[256];
value *arg_stack_low, *arg_stack_high;
value *ret_stack_low, *ret_stack_high;
value tail = 0;

static inline void modify(value *x, value y)
{
  *x = y;
}

value alloc_with_hd(u32 size, hd_t hd)
{
  value block = (value)malloc((size+2)*sizeof(value));
  *(value *)block = hd;
  Field(block, -1) = tail;
  if (tail)
    Field(tail, -1) ^= block;
  tail = block;
  return block;
}

value alloc(u8 tag, u32 size)
{
  return alloc_with_hd(size, Make_header(tag, size));
}

value alloc_block(value env, u32 nmore)
{
  value newenv = alloc(Tag_val(env), Wosize_val(env) + nmore);
  memcpy((char*)newenv + (2+nmore)*sizeof(value),
         (char*)env + 2*sizeof(value), Bosize_val(env));
  return (value)newenv;
}

void disasm(code_t pc)
{
  u8 op = *pc++;
  printf("%s ", name_of_instructions[op]);
  switch (op) {
  case ACCESS:
  case DUMMY:
  case ENDLET:
  case GETFIELD:
  case SETFIELD:
  case UPDATE:
    printf("%d", *pc++);
    break;
  case CCALL1:
  case CCALL2:
  case CCALL3:
  case CCALL4:
    printf("%s", name_of_cprims[*pc++]);
    break;
  case CONSTINT8:
    printf("%d", *(i8*)pc++);
    break;
  case GETGLOBAL:
  case SETGLOBAL:
    if ((value)pc & 1) pc++;
    printf("[%d]", pu16(pc));
    pc += 2;
    break;
  case CONSTINT16:
    if ((value)pc & 1) pc++;
    printf("[%d]", pi16(pc));
    pc += 2;
    break;
  case BRANCH:
  case BRANCHIF:
  case BRANCHIFEQ:
  case BRANCHIFGE:
  case BRANCHIFGT:
  case BRANCHIFLE:
  case BRANCHIFLT:
  case CUR:
    if ((value)pc & 1) pc++;
    printf("0x%08x", pc+pi16(pc));
    pc += 2;
    break;
  case BRANCHIFNEQTAG:
    printf("%d ", *pc++);
    if ((value)pc & 1) pc++;
    printf("0x%08x", pc+pi16(pc));
    pc += 2;
    break;
  }
  putchar('\n');
}

bool touch(value x)
{
  u32 size, color;
  switch (Tag_val(x)) {
  case Closure_tag:
    if (Color_val(x))
      return true;
    Hd_val(x) = Set_color_val(x, 1);
    return false;
  case String_tag:
    Hd_val(x) |= 1 << Gcsize_offset;
    return true;
  case Array_tag:
    size = array_length(x);
    color = Field(x, 0);
    if (color < size) {
      Field(x, 0)++;
      return false;
    }
    if (! size)
      Field(x, 0) = 1;
    return true;
  default:
    size = Wosize_val(x);
    color = Color_val(x);
    if (color < size) {
      Hd_val(x) = Set_color_val(x, color+1);
      return false;
    }
    if (! size)
      Hd_val(x) = Set_color_val(x, 1);
    return true;
  }
}

value get_cur_field(value x)
{
  switch (Tag_val(x)) {
  case Closure_tag:
    return Env_val(x);
  case Array_tag:
    return Field(x, Field(x, 0));
  default:
    return Field(x, Color_val(x)-1);
  }
}

void set_cur_field(value x, value y)
{
  switch (Tag_val(x)) {
  case Closure_tag