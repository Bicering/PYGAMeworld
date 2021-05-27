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
  case Closure_tag:
    Env_val(x) = y;
    break;
  case Array_tag:
    Field(x, Field(x, 0)) = y;
    break;
  default:
    Field(x, Color_val(x)-1) = y;
    break;
  }
}

bool is_fresh(value x)
{
  switch (Tag_val(x)) {
  case String_tag:
    return String_color_val(x) == 0;
  case Array_tag:
    return Field(x, 0) == 0;
  default:
    return Color_val(x) == 0;
  }
}

void reset_color(value x)
{
  switch (Tag_val(x)) {
  case String_tag:
    Hd_val(x) &= ~ (1 << Gcsize_offset);
    break;
  case Array_tag:
    Field(x, 0) = 0;
    break;
  default:
    Hd_val(x) = Set_color_val(x, 0);
    break;
  }
}

void schorr_waite(value x)
{
  if (! Is_block(x) || ! x || ! is_fresh(x)) return;
  value p = 0, y;
  for(;;) {
    if (touch(x)) {
      if (! p) return;
      y = x;
      x = p;
      p = get_cur_field(x);
      set_cur_field(x, y);
    } else {
      y = get_cur_field(x);
      if (Is_block(y) && y && is_fresh(y)) {
        set_cur_field(x, p);
        p = x;
        x = y;
      }
    }
  }
}

void gc(value acc, value env, value *asp, value *rsp, struct trap_frame *tp)
{
  for (value *p = asp; p < arg_stack_high; p++)
    schorr_waite(*p);
  for (value *p = rsp; p < ret_stack_high; ) {
    if (p == (value*)tp) {
      schorr_waite(tp->env);
      tp = tp->tp;
      p = (value*)((char*)p-sizeof(struct trap_frame));
    } else {
      schorr_waite(((struct return_frame*)p)->env);
      p++;
    }
  }
  schorr_waite(global_value);
  schorr_waite(acc);
  schorr_waite(env);
  value x = 0, y = tail, z;
  while (y) {
    z = Field(y, -1) ^ x;
    if (is_fresh(y)) {
      if (x)
        Field(x, -1) ^= y ^ z;
      else
        tail = z;
      if (z)
        Field(z, -1) ^= y ^ x;
      // printf("+ free %08x\n", y);
      free((void*)y);
      y = z;
    } else {
      reset_color(y);
      x = y;
      y = z;
    }
  }
}

#define FAILED_TO_OPEN -1
#define BAD_MAGIC -2
#define TRUNCATED_FILE -3
#define INVALID_EXE -4
#define SYSERROR -5

#define UNCAUGHT_EXCEPTION -6

int interpret(code_t code)
{
  value acc = Val_int(0), env = Atom(0),
        *asp = arg_stack_high, *rsp = ret_stack_high;
  struct trap_frame *tp = NULL;
  code_t pc = code;
  value tmp;

#define retsp ((struct return_frame *)rsp)
#define Push_ret_frame ( rsp = (value*)((char*)rsp-sizeof(struct return_frame)) )
#define Pop_ret_frame ( rsp = (value*)((char*)rsp+sizeof(struct return_frame)) )
#define trapsp ((struct trap_frame *)rsp)
#define Push_trap_frame ( rsp =