all: test_20210808.out
%.out: %.Rmd
	R --no-save -e 'library(rmarkdown);render("$<","pdf_document")'

