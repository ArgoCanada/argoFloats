## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2
library(argoFloats)
context("useAdjusted() on BGC (synthetic) data")

test_that("useAdjusted() on BGC (synthetic) delayed-mode data", {
          a <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"), quiet=TRUE)
          a2 <- useAdjusted(a)
          expect_equal(-6.69775332282, mean(a[[1]][["oxygen"]] - a2[[1]][["oxygen"]],na.rm=TRUE))
          # Create fake data, in which oxygen mode is "R", so we will fall back
          # to raw.  This is done by altering the parameterDataMode, which for
          # this file was originally equal to "DDDDARR", for variables PRES TEMP
          # PSAL DOXY CHLA BBP700 NITRATE.  We can do this by changing the last
          # 'D' entry to 'R", to make it look as though the oxygen data are in
          # real-time.  Then, fallback=TRUE should return 'oxygen' from
          # the original, instead of copying over the oxygenAdjusted values.
          aFake <- a
          aFake@data$argos[[1]]@metadata$parameterDataMode <- "DDDRARR"
          a3 <- useAdjusted(aFake, fallback=TRUE)
          expect_equal(a3[[1]][["oxygen"]], a[[1]][["oxygen"]])
})
