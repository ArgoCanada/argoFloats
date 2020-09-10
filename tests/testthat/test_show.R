## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)
library(testthat)

context("show")

test_that("show", {
          data("index")
          expect_output(print(index), "argoFloats object of type \"index\" with 978 items")
})

