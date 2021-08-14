VSN=profiling
$(VSN).pdf: $(VSN).Rmd
	R --no-save -e 'library(rmarkdown); render("$<", "pdf_document")'
