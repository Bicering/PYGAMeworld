.PHONY: force test

CFLAGS += -std=gnu11 -g3
CFLAGS += -m32

SRC := $(wildcard *.ml) lexer.mll parser.mly opcode.ml cprim.ml
C := $(addprefix runtime/,main.c compare.c error.c instruct.c io.c main.c prim.c str.c)
HD := $(addprefix runtime/,common.h error.h instruct.h io.h jumptable.h prim.h str.h value.h)
CYAN := '\e[1;36m'
GREEN := '\e[1;32m'
RST := '\e[0m'

all: camlfwc camlfwod camlfwrun

camlfwc: ma