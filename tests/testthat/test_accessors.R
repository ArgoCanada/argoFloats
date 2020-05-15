## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)

context("built-in datasets")
data(index)
data(indexBgc)
data(indexMerged)

test_that("accessors work on 'index' data file", {
          expect_equal(index[["file", 1]], "aoml/1901584/profiles/R1901584_124.nc")
          expect_equal(index[["file", 1:2]], c("aoml/1901584/profiles/R1901584_124.nc", "aoml/1901584/profiles/R1901584_125.nc"))
          expect_equal(index[[1]]$file, index[["file"]][1])
          expect_equal(head(index[["longitude"]],3), c(-76.231, -76.024, -76.320))
          expect_equal(head(index[["latitude"]],3), c(27.705, 27.773, 28.064))
})

test_that("access float ID", {
          expect_equal("4900227", index[['ID', 953]])
          expect_equal(c("1901584", "1901584", "1901584"), index[['ID', 1:3]])
          expect_equal(c("1901584", "1901584", "1901584"), head(index[['ID']], 3))
})

test_that("access float profile number", {
          expect_equal(c("124", "125", "126"), index[["profile", 1:3]])
          expect_equal(c("124", "125", "126"), head(index[["profile"]], 3))
})

