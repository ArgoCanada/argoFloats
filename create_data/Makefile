all:
	echo "This Makefile is disabled, to avoid altering existing datasets. Experts know what to do..."
clean:
	-rm *~ *.rda *.out
profiles:
	Rscript create_profiles.R > create_profiles.out

indices:
	make index
	make indexBgc
	#make indexMerged
	make indexSynthetic
install_indices:
	make install_index
	make install_indexBgc
	#make install_indexMerged
	make install_indexSynthetic


index: force
	Rscript create_index.R > create_index.out
indexBgc: force
	Rscript create_indexBgc.R > create_indexBgc.out
indexSynthetic: force
	Rscript create_indexSynthetic.R > create_indexSynthetic.out
indexDeep: force
	Rscript create_indexDeep.R > create_indexDeep.out
install_index:
	cp index.rda ../data
install_indexBgc:
	cp indexBgc.rda ../data
#install_indexMerged:
	#cp indexMerged.rda ../data
install_indexSynthetic:
	cp indexSynthetic.rda ../data
install_indexDeep:
	cp indexDeep.rda ../data
force:

