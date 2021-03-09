## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2
library(argoFloats)
context("useAdjusted() on BGC (synthetic) data")

test_that("useAdjusted() on BGC (synthetic) delayed-mode data", {
          a <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"), quiet=TRUE)
          a2 <- useAdjusted(a)
          expect_equal(-6.69775332282, mean(a[[1]][["oxygen"]] - a2[[1]][["oxygen"]],na.rm=TRUE))
          # Create fake data, in which oxygen mode is "R", so we will fall back to raw
          aFake <- a
          aFake@data$argos[[1]]@metadata$parameterDataMode <- "DDDRARR" # Original is "DDDDARR"
          a3 <- useAdjusted(aFake, fallback=TRUE)
          expect_equal(a3[[1]][["oxygen"]], a[[1]][["oxygen"]])
})
