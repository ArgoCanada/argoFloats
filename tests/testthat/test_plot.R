## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(argoFloats)
library(testthat)

context("plot")

test_that("plot", {
          data("index")
          expect_silent(plot(index, which="map", bathymetry=FALSE))
})

