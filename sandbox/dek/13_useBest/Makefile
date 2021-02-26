all: $(patsubst %.R,%.out,$(wildcard *.R))
%.out: %.R
	Rscript $< &> $@
clean:
	-rm *.out *.png *.pdf *~
