## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)
library(testthat)

context("show")

test_that("show", {
          if (canDownload()){
          data("index")
          index1 <- expect_message(subset(index,1:5,"Kept 5 profiles (0.511%)"))
          profiles <- getProfiles(index1)
          expect_output(print(profiles), "argoFloats object of type \"profiles\" with 5 items")
          expect_output(print(index), "argoFloats object of type \"index\" with 978 items")
          argos <- expect_output(expect_warning(readProfiles(profiles),
                                                "Of 5 profiles read, 5 have"), "|===")
}})

