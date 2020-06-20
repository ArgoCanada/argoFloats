## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)
context("using 'adjusted' data")

test_that("useAdjusted() test",
          {
              for (nc in c("D4900785_048.nc", "SD5903586_001.nc")) {
                  filename <- system.file("extdata", nc, package="argoFloats")
                  ## raw
                  r <- readProfiles(filename, silent=TRUE)
                  expect_equal(r[["salinity"]], r[["PSAL"]])
                  expect_equal(r[["temperature"]], r[["TEMP"]])
                  expect_equal(r[["pressure"]], r[["PRES"]])
                  expect_equal(r[["salinityAdjusted"]], r[["PSAL_ADJUSTED"]])
                  expect_equal(r[["temperatureAdjusted"]], r[["TEMP_ADJUSTED"]])
                  expect_equal(r[["pressureAdjusted"]], r[["PRES_ADJUSTED"]])
                  ## adjusted
                  a <- useAdjusted(r)
                  expect_equal(a[["salinity"]], a[["PSAL_ADJUSTED"]])
                  expect_equal(a[["temperature"]], a[["TEMP_ADJUSTED"]])
                  expect_equal(a[["pressure"]], a[["PRES_ADJUSTED"]])
                  expect_equal(a[["salinityUnadjusted"]], a[["PSAL"]])
                  expect_equal(a[["temperatureUnadjusted"]], a[["TEMP"]])
                  expect_equal(a[["pressureUnadjusted"]], a[["PRES"]])
                  for (field in c("salinity", "temperature", "pressure")) {
                      expect_equal(r[[paste0(field, "Adjusted")]], a[[field]])
                      expect_equal(r[[field]], a[[paste0(field, "Unadjusted")]])
                      expect_equal(r[[paste0(field, "AdjustedError")]], a[[paste0(field, "AdjustedError")]])
                  }
              }
          }
)
