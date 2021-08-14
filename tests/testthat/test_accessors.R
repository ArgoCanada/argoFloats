## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(argoFloats)

context("built-in datasets")
data(index)

test_that("accessors work on 'index' data file",
          {
              expect_equal(ncol(index@data$index), 8)
              expect_equal(ncol(index[["index"]]), 8)
              expect_equal(index[["time"]], index[["date"]])
              expect_equal(index[["time_update"]], index[["date_update"]])
          }
)

test_that("access float ID",
          {
              expect_equal(gsub(".*/[RD](.*)_.*.nc$", "\\1", index@data$index$file[1:3]), index[["ID", 1:3]])
          }
)

test_that("access float cycle",
          {
              expect_equal(gsub(".*_(.*).nc$", "\\1", index@data$index$file[1:3]), index[["cycle", 1:3]])
          }
)

test_that("access float cycle number/ profile",
          {
              filename <- system.file("extdata", "D4900785_048.nc", package="argoFloats")
              argos <- expect_silent(readProfiles(filename))
              expect_equal(argos[["cycle"]], "048")
              expect_equal(argos[["cycle",1]], "048")
          }
)

test_that("access within cycles",
          {
              filename <- system.file("extdata", "D4900785_048.nc", package = "argoFloats")
              a <- expect_silent(readProfiles(filename))
              expect_error(a[["longitude", "wrong"]], "requires that j be \"byLevel\", not \"wrong\"")
              longitude <- expect_silent(a[["longitude"]])
              expect_true(is.list(longitude))
              expect_equal(1, length(longitude))
              longitude <- expect_silent(a[["longitude", "byLevel"]])
              salinity <- expect_silent(a[["salinity"]])
              expect_equal(dim(longitude), dim(salinity))
              expect_equal(a[["cycle"]], "048")
              expect_equal(a[["cycle", 1]], "048")
              expect_equal(a[["ID", 1]], "4900785")
          }
)

test_that("historyQCTest length and (trimmed) contents for issue 136",
          {
              ## https://github.com/ArgoCanada/argoFloats/issues/136
              filename <- system.file("extdata", "D4900785_048.nc", package = "argoFloats")
              a <- expect_silent(readProfiles(filename))
              a1 <- a[[1]]
              ## We test two ways of storing the HISTORY_QC_TEST item as named in the NetCDF file,
              ## because the camelCase variety only became valid in late June of 2020, and then
              ## only in the github vrsion, not yet the CRAN version.
              test1 <- a1[["historyQCTest"]]
              test2 <- a1[["HISTORY_QC_TEST"]]
              nc <- ncdf4::nc_open(filename)
              qcn <- ncdf4::ncvar_get(nc, "HISTORY_QCTEST")
              if (length(test1)) {
                  expect_equal(6, length(test1))
                  expect_equal(test1, gsub("[ ]*", "", qcn))
              } else if (length(test2)) {
                  expect_equal(6, length(test2))
                  expect_equal(test2, gsub("[ ]*", "", qcn))
              }
          }
)

test_that("access length",
          {
              #expect_equal(index[["length"]], 978)
              filename <- system.file("extdata", "D4900785_048.nc", package="argoFloats")
              argos <- expect_silent(readProfiles(filename))
              expect_equal(argos[["length"]], 1)
          }
)

test_that("test access argos type",
          {
              filename <- system.file("extdata", "D4900785_048.nc", package="argoFloats")
              argos <- expect_silent(readProfiles(filename))
              expect_equal(argos[["latitude"]][[1]], 27.916)
          }
)

test_that("test error messages",
          {
              data("index")
              expect_error(index[["parameters"]], "there are no parameters for core Argo index objects. Try BGC, Merged, or Synthetic Argo.")
              expect_error(index[["dog"]], "no \"dog\" in an object of type=\"index\" and subtype=\"cycles\"")
          }
)

test_that("access to metadata",
          {
              index1 <- expect_silent(index[['metadata']])
              expect_equal(index1[[1]], "index")
          }
)

