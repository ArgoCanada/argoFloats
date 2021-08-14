R=$(patsubst %.R,%.out,$(wildcard *.R))
RMD=$(patsubst %.Rmd,%.pdf,$(wildcard *.Rmd))
all: $(R) $(RMD)
%.out: %.R
	Rscript $< &> $@
%.pdf: %.Rmd
	R --no-save -e 'library(rmarkdown); render("$<", "pdf_document")'

clean:
	-rm *.out *.png *.pdf *~
