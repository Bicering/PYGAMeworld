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
     *y = (u8*)((value*)s2+2);
  for (u32 len = len1 < len2 ? len1 : len2; len; len--) {
    if (*x != *y)
      return *x < *y ? -1 : 1;
    x++;
    y++;
  }
  if (len1 < len2) return -1;
  if (len1