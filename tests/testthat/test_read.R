## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)

context("reading data")

## FIXME: add test with 'adjusted' argument (if we retain that trial feature)
test_that("read a local NetCDF file", {
          a <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")),
                              "Of 1 profiles read, 1 has")
          expect_equal(1L, a[["length"]])
          expect_equal(head(a[[1]][["oxygen"]], 6L),
                       structure(c(200.108535766602, NA, 199.907455444336, 199.955490112305, NA, NA),
                                 .Dim = c(6L, 1L)))
          expect_equal(head(a[[1]][["oxygenFlag"]],6L),
                       structure(c(1, NA, 1, 1, NA, NA), .Dim = c(6L, 1L)))
          expect_equal(mean(unlist(a[["oxygen"]]),na.rm=TRUE), 35.7413040649456)
})

