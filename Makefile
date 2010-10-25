
CC=gcc
LD=gcc

CFLAGS=-O0 -Wall -g3

CEXECUTABLES=betterop betterop_bench outerprod outerprod_bench
GENCFILES=betterop.c betterop_bench.c outerprod.c outerprod_bench.c
GENHFILES=betterop.h betterop_bench.h outerprod.h outerprod_bench.h

all: $(CEXECUTABLES)

$(GENCFILES) $(GENHFILES): atom_op.hs
	runhaskell atom_op.hs

betterop: betterop.c betterop.h
	$(CC) $(CFLAGS) $< -o $@

betterop_bench: betterop_bench.c betterop_bench.h
	$(CC) $(CFLAGS) $< -o $@

outerprod: outerprod.c outerprod.h
	$(CC) $(CFLAGS) $< -o $@

outerprod_bench: outerprod_bench.c outerprod_bench.h
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm $(GENCFILES) $(GENHFILES) $(CEXECUTABLES)

