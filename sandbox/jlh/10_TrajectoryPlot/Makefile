all: $(patsubst %.R,%.out,$(wildcard *.R))
%.out: %.R
	R --no-save < $< &> $@
clean:
	-rm marmap_*csv *.out *.png *.pdf *~
