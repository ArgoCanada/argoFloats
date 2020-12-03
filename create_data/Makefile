all:
	echo "This Makefile is disabled, to avoid altering existing datasets. Experts know what to do..."
clean:
	-rm *~ *.rda *.out
profiles:
	Rscript create_profiles.R > create_profiles.out
indices: force
	Rscript create_index.R > create_index.out
	Rscript create_indexBgc.R > create_indexBgc.out
	Rscript create_indexDeep.R > create_indexDeep.out
	Rscript create_indexSynthetic.R > create_indexSynthetic.out
install_indices: force
	cp index.rda ../data
	cp indexBgc.rda ../data
	cp indexDeep.rda ../data
	cp indexSynthetic.rda ../data
force:

