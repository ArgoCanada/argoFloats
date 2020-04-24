## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(argoFloats)
library(testthat)

context("summary")

test_that("summary", {
          data("index")
          expect_output(summary(index), "argoFloats summary")
})

