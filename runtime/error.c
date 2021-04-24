#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include "error.h"
#include "value.h"

void raise_with_string(u8 tag, const char *msg)
{
  fprintf(stderr, "%s\n", msg);
  abort();
}

void not_implemented(const char *msg)
{
  fprintf(stderr, "%s\n", msg);
  abort();
}

void invalid_argument(const char *msg)
{
  ra