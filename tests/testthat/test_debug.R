## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)

context("argoFloatsDebug")

test_that("argoFloatsDebug", {
    expect_output(argoFloatsDebug(1,'red',style='red'),"red")
    expect_output(argoFloatsDebug(1,"plain"),"plain")
    expect_output(argoFloatsDebug(3,"red",style="red"),"red")
    expect_output(argoFloatsDebug(3, "red", style="bold"), "red")
    expect_output(argoFloatsDebug(3, "red", style="italic"), "red")
    expect_output(argoFloatsDebug(3, "red", style="green"), "red")
    expect_output(argoFloatsDebug(3, "red", style="blue"), "red")
    expect_silent(argoFloatsDebug(0))
          })
