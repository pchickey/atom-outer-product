
CC=gcc
LD=gcc

CFLAGS=-O3 -Wall -g3

CEXECUTABLES=betterop betterop_bench outerprod outerprod_bench op_triangle op_triangle_bench
GENCFILES=betterop.c betterop_bench.c outerprod.c outerprod_bench.c op_triangle.c
GENHFILES=betterop.h betterop_bench.h outerprod.h outerprod_bench.h op_triangle.h

all: $(CEXECUTABLES)

$(GENCFILES) $(GENHFILES): atom_outer_product.hs
	runhaskell atom_outer_product.hs

betterop: betterop.c betterop.h
	$(CC) $(CFLAGS) $< -o $@

betterop_bench: betterop_bench.c betterop_bench.h
	$(CC) $(CFLAGS) $< -o $@

outerprod: outerprod.c outerprod.h
	$(CC) $(CFLAGS) $< -o $@

outerprod_bench: outerprod_bench.c outerprod_bench.h
	$(CC) $(CFLAGS) $< -o $@

op_triangle: op_triangle.c op_triangle.h
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm $(GENCFILES) $(GENHFILES) $(CEXECUTABLES)

