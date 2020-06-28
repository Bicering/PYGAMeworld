.PHONY: force test

CFLAGS += -std=gnu11 -g3
CFLAGS += -m32

SRC := $(wildcard *.ml) lexer.mll parser.mly opcode.ml cprim.ml
C := $(addprefix runtime/,main.c compare.c error.c instruct.c io.c 