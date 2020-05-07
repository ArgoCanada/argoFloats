## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)

context("built-in datasets")
data(index)
data(indexBgc)
data(indexMerged)

test_that("accessors work on 'index' data file", {
          expect_equal(index[["profile", 1]], "aoml/1901584/profiles/R1901584_124.nc")
          expect_equal(index[["profile", 1:2]], c("aoml/1901584/profiles/R1901584_124.nc", "aoml/1901584/profiles/R1901584_125.nc"))
          expect_equal(index[[1]]$file, index[["file"]][1])
})

