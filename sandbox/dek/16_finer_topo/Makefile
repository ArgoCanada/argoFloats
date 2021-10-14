all: create_topoWorld.out
%.out: %.R
	Rscript $< > $@
clean: force
	-rm -f *~ *.out *.png *.nc *.rda
force:

