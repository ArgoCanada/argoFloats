## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)

context("show")

test_that("show", {
        data("index")
        expect_output(show(index), "argoFloats object of type \"index\"")
        index1 <- expect_message(subset(index,1:5,"Kept 5 cycles (0.511%)"))
        a <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")), "Of 1 profiles read")
        expect_output(show(a), "argoFloats object of type \"argos\" with 1 items")
        expect_output(print(a), "argoFloats object of type \"argos\" with 1 items")
    })

