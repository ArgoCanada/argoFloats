## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(argoFloats)

context("built-in index datasets")
data(index)
data(indexBgc)
data(indexSynthetic)


test_that("index has expected column names", {
          expect_equal(names(index[["index"]]), c("file", "date", "latitude", "longitude", "ocean", "profiler_type",
                                                  "institution", "date_update"))
})

test_that("indexBgc has expected column names", {
          expect_equal(names(indexBgc[["index"]]), c("file", "date", "latitude", "longitude", "ocean", "profiler_type",
                                                     "institution", "parameters", "parameter_data_mode", "date_update"))
})

test_that("indexSynthetic has expected column names", {
          expect_equal(names(indexSynthetic[["index"]]), c("file", "date", "latitude", "longitude", "ocean", "profiler_type",
                                                           "institution", "parameters", "parameter_data_mode", "date_update"))
})

