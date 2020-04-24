## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)

context("built-in datasets")
data(index)
data(indexBgc)
data(indexMerged)
test_that("index is of expected dimension", {
          expect_equal(dim(index[["index"]]), c(953, 8))
          expect_equal(names(index[["index"]]), c("file", "date", "latitude", "longitude", "ocean", "profiler_type",
                                                  "institution", "date_update"))
})

test_that("indexBgc is of expected dimension", {
          expect_equal(dim(indexBgc[["index"]]), c(39, 10))
          expect_equal(names(indexBgc[["index"]]), c("file", "date", "latitude", "longitude", "ocean", "profiler_type",
                                                     "institution", "parameters", "param_data_mode", "date_update"))
})

test_that("indexMerged is of expected dimension", {
          expect_equal(dim(indexMerged[["index"]]), c(39, 10))
          expect_equal(names(indexMerged[["index"]]), c("file", "date", "latitude", "longitude", "ocean", "profiler_type",
                                                     "institution", "parameters", "param_data_mode", "date_update"))
})


test_that("accessors work on 'index' data file", {
          expect_equal(index[["profile", 1]], "aoml/1901584/profiles/R1901584_124.nc")
          expect_equal(index[["profile", 1:2]], c("aoml/1901584/profiles/R1901584_124.nc", "aoml/1901584/profiles/R1901584_125.nc"))
})


