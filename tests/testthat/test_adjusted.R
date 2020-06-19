## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)
context("processing of QC flags")

test_that("useAdjusted() test on whole object",
          {
              #filename <- system.file("extdata", "SR2902204_131.nc", package="argoFloats")
              filename <- system.file("extdata", "D4900785_048.nc", package="argoFloats")
              raw <- readProfiles(filename)
              adjusted <- useAdjusted(raw)
              for (field in c("salinity", "temperature", "pressure")) {
                  expect_equal(raw[[paste0(field, "Adjusted")]], adjusted[[field]])
                  expect_equal(raw[[field]], adjusted[[paste0(field, "Unadjusted")]])
                  ##? expect_equal(raw[[paste0(field, "AdjustedError")]], adjusted[[paste0(field, "AdjustedError")]])
                  ##? expect_equal(raw[[field]], adjusted[[paste0(field, "Error")]])
              }
          }
)
