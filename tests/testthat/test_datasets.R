## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)

context("built-in index datasets")
data(index)
data(indexBgc)
#data(indexMerged)
data(indexSynthetic)

#test_that("indexMerged and indexSynthetic files correspond", {
#          m <- indexMerged[["file"]]
#          s <- indexSynthetic[["file"]]
#          ## MR and MD files both become SD files
#          expect_equal(gsub("MR", "SD", gsub("MD", "SD", m)), s)
#})

test_that("index is of expected dimension", {
          #expect_equal(dim(index[["index"]]), c(978, 8))
          expect_equal(names(index[["index"]]), c("file", "date", "latitude", "longitude", "ocean", "profiler_type",
                                                  "institution", "date_update"))
})

test_that("indexBgc is of expected dimension", {
          #expect_equal(dim(indexBgc[["index"]]), c(39, 10))
          expect_equal(names(indexBgc[["index"]]), c("file", "date", "latitude", "longitude", "ocean", "profiler_type",
                                                     "institution", "parameters", "parameter_data_mode", "date_update"))
})

#test_that("indexMerged is of expected dimension", {
#         expect_equal(dim(indexMerged[["index"]]), c(39, 10))
#          expect_equal(names(indexMerged[["index"]]), c("file", "date", "latitude", "longitude", "ocean", "profiler_type",
#                                                     "institution", "parameters", "param_data_mode", "date_update"))
#})


test_that("accessors work on 'index' data file", {
          expect_equal(index[["file", 1]], "aoml/1901584/profiles/R1901584_124.nc")
          expect_equal(index[["file", 1:2]], c("aoml/1901584/profiles/R1901584_124.nc", "aoml/1901584/profiles/R1901584_125.nc"))
})


