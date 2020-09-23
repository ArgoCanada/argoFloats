## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)
library(testthat)

context("show")

test_that("show", {
        data("index")
        index1 <- expect_message(subset(index,1:5,"Kept 5 profiles (0.511%)"))
        expect_output(print(index), "argoFloats object of type \"index\" with 978 items")
        a <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")))
        expect_output(print(a), "argoFloats object of type \"argos\" with 1 items")
    })

