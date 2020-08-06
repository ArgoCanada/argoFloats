## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)

context("built-in datasets")
data(index)

test_that("accessors work on 'index' data file", {
          expect_equal(index[[1]],
                       structure(list(file="aoml/1901584/profiles/R1901584_124.nc",
                                      date=structure(1438611452,
                                                     class=c("POSIXct", "POSIXt"),
                                                     tzone="UTC"),
                                      latitude=27.705,
                                      longitude=-76.231,
                                      ocean="A",
                                      profiler_type=851L,
                                      institution="AO",
                                      date_update=structure(1570484144, class=c("POSIXct", "POSIXt"), tzone="UTC")),
                                 row.names=110587L,
                                 class="data.frame"))
          expect_equal(index[["file", 1]], "aoml/1901584/profiles/R1901584_124.nc")
          expect_equal(index[["file", 1:2]], c("aoml/1901584/profiles/R1901584_124.nc", "aoml/1901584/profiles/R1901584_125.nc"))
          expect_equal(index[[1]]$file, index[["file"]][1])
          expect_equal(head(index[["longitude"]],3), c(-76.231, -76.024, -76.320))
          expect_equal(head(index[["latitude"]],3), c(27.705, 27.773, 28.064))
})

test_that("access float id", {
          expect_equal("4900227", index[['id', 953]])
          expect_equal(c("1901584", "1901584", "1901584"), index[['id', 1:3]])
          expect_equal(c("1901584", "1901584", "1901584"), head(index[['id']], 3))
})

test_that("access float profile number", {
          expect_equal(c("124", "125", "126"), index[["cycle", 1:3]])
          expect_equal(c("124", "125", "126"), head(index[["cycle"]], 3))
})

test_that("access within profiles", {
          filename <- system.file("extdata", "D4900785_048.nc", package="argoFloats")
          ## FIXME: when I run the next line interactively, or by the "Run tests" 
          ## action for an editor window opened to this file, then it works.  However,
          ## when I run it as a "R CMD check", it fails.  I do not know why, and
          ## will look into this in more detail at some point, but, for now, I
          ## am commenting-out the line.
          ##a <- expect_silent(readProfiles(filename))
          a <- readProfiles(filename)
          expect_error(a[["longitude", "wrong"]], "requires that j be \"byLevel\", not \"wrong\"")
          longitude <- expect_silent(a[["longitude"]])
          expect_true(is.list(longitude))
          expect_equal(1, length(longitude))
          longitude <- expect_silent(a[["longitude", "byLevel"]])
          salinity <- expect_silent(a[["salinity"]])
          expect_equal(dim(longitude), dim(salinity))
          expect_equal(a[['cycle']], "048")
})

test_that("historyQCTest length and (trimmed) contents for issue 136", {
          ## https://github.com/ArgoCanada/argoFloats/issues/136
          filename <- system.file("extdata", "D4900785_048.nc", package="argoFloats")
          a <- expect_silent(readProfiles(filename))
          a1 <- a[[1]]
          ## We test two ways of storing the HISTORY_QC_TEST item as named in the netcdf file,
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
})
