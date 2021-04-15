#include "error.h"
#include "value.h"
#include "str.h"

// comparison on nonrecursive structures
intptr_t cmp_value(value v1, value v2)
{
  if (v1 == v2) return 0;
  if (Is_int(v1) || Is_int(v2)) return Int_val(v1)-Int_val(v2);
  u8 t1 = Tag_val(v1), t2 = Tag_val(v2);
  if (t1 != t2) return (intptr_t)t1-t2;
  switch (t1) {
  case String_t