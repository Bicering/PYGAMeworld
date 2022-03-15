#include "str.h"

u32 string_length(value s)
{
  u32 size = String_wosize_val(s);
  return size*sizeof(value) -