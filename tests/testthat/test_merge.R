## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)
context("merge data")
data(index)

test_that("test merging data", {
    N <- 104
    C <- expect_message(subset(index, circle=list(longitude=-77.5, latitude=27.5, radius=50)), paste("Kept", N, "profiles"))
    R <- expect_message(subset(index, rectangle=list(longitude=c(-76.5, -76), latitude=c(26.5, 27.5))), "Kept 59 profiles")
    RC <- expect_silent(merge(C, R))
    #expect_equal(dim(RC[["index"]]), c(156, 8))
})

test_that("stop messages", {
    argos <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")))
    RA <- expect_error(merge(argos,R, "Error: in merge,argoFloats-method(): 'x' was not created with getIndex()."))
    RA <- expect_error(merge(R,argos, "Error: in merge,argoFloats-method():
 'y' was not created with getIndex(). "))
})

