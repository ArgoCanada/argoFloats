## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)
context("merge data")
data(index)
data(indexBgc)

test_that("test merging core data",
          {
              C <- expect_message(subset(index, circle=list(longitude=-77.5, latitude=27.5, radius=50)), "Kept [0-9]* cycles")
              R <- expect_message(subset(index, rectangle=list(longitude=c(-76.5, -76), latitude=c(26.5, 27.5))), "Kept [0-9]* cycles")
              RC <- expect_silent(merge(C, R))
          }
)

test_that("test merging core and Bgc data",
          {
              M <- merge(index, indexBgc)
              expect_equal(sum(is.na(M[["parameters"]])), index[["length"]])
              expect_equal(sum(!is.na(M[["parameters"]])), indexBgc[["length"]])
              expect_silent(plot(M, which="map", bathymetry=FALSE))
          }
)

test_that("stop messages",
          {
              argos <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")))
              RA <- expect_error(merge(argos,R, "Error: in merge,argoFloats-method(): 'x' was not created with getIndex()."))
              RA <- expect_error(merge(R,argos, "Error: in merge,argoFloats-method(): 'y' was not created with getIndex()."))
          }
)

