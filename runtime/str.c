#include "str.h"

u32 string_length(value s)
{
  u32 size = String_wosize_val(s);
  return size*sizeof(value) - ((uvalue)Field(s, size-1) >> sizeof(value)*8-8);
}

int string_compare(value s1, value s2)
{
  u32 len1 = string_length(s1),
      len2 = string_length(s2);
  u8 *x = (u8*)((value*)s1+2),
     *y