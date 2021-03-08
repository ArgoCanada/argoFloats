## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2
library(argoFloats)
context("useAdjusted() on BGC (synthetic) data")

test_that("useAdjusted() on BGC (synthetic) delayed-mode data", {
          a <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"), quiet=TRUE)
          aNA <- useAdjusted(a)
})


test_that("useAdjusted() on BGC (synthetic) delayed-mode data", {
          a <- readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats"), quiet=TRUE)
          aNA <- useAdjusted(a)
})
