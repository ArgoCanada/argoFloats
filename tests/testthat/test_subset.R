## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)

context("subset")

test_that("subset by circle", {
          data("index")
          indexAI <- expect_silent(subset(index, circle=list(longitude=-77.06, latitude=26.54, radius=100)))
          expect_equal(dim(indexAI[["index"]]), c(304, 8))
})

