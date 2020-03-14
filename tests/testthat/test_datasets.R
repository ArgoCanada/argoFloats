## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)

context("built-in datasets")

test_that("index is of expected length", {
          data("index")
          expect_equal(dim(index@data$index), c(1783, 8))
})

test_that("subset works as expected", {
          data("index")
          indexSI <- expect_silent(subset(index, circle=list(longitude=-59.91, latitude=43.93, radius=200)))
          expect_equal(dim(indexSI@data$index), c(770, 8))
})

