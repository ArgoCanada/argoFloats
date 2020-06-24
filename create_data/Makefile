all:
	echo "This Makefile is disabled, to avoid altering existing datasets. Experts know what to do..."
clean:
	-rm *~ *.rda *.out
profiles:
	Rscript create_profiles.R > create_profiles.out
index: force
	Rscript create_index.R > create_index.out
indexBgc: force
	Rscript create_indexBgc.R > create_indexBgc.out
indexMerged: force
	Rscript create_indexMerged.R > create_indexMerged.out
indexSynthetic: force
	Rscript create_indexSynthetic.R > create_indexSynthetic.out
install_index:
	cp index.rda ../data
install_indexBgc:
	cp indexBgc.rda ../data
install_indexMerged:
	cp indexMerged.rda ../data
install_indexSynthetic:
	cp indexSynthetic.rda ../data
force:

