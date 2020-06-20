library(argoFloats)
source("can_download.R")
library(testthat)

context("showing which QC tests were performed and failed")

test_that("showing QC tests", {
          if (canDownload()) {
              data('index')
              subset <- subset(index, 1)
              indexq <- expect_message(subset(index, 1),
                                       "Kept 1 profiles \\(0.105%\\)")
              profiles <- getProfiles(subset)
              argos <- expect_warning(readProfiles(profiles), "^Of 1 profiles read")
              argos1 <- argos[[1]]
              expect_output(showQCTests(argos[[1]]), "^Tests performed: 2 4 5 6 7 8 15 16 17 18 19\n[ ]*Tests failed:    2[ ]*$")
          }
})
