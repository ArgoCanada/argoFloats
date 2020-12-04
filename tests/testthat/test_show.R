## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)

context("show")

test_that("show", {
        data("index")
        expect_output(show(index), "argoFloats object of type \"index\"")
        index1 <- expect_message(subset(index,1:5,"Kept 5 profiles (0.511%)"))
        file <- system.file("extdata", "SR2902204_131.nc", package="argoFloats")
        a <- expect_warning(readProfiles(file), "Of 1 profiles read")
        expect_output(print(a), "argoFloats object of type \"argos\" with 1 items")
    })

