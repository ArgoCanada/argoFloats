## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)
context("bgc download")

test_that("getIndex()",
          {
              tempFile <- tempfile()
              dir.create(tempFile)
              i <- expect_silent(getIndex(filename="argo_bio-profile_index.txt.gz", quiet=TRUE, destdir=tempFile))
              i2 <- expect_error(getIndex(filename="dog","filename=\"dog\" doesn't exist. Try one of these: \"argo\", \"core\", \"bgc\", \"bgcargo\", or \"synthetic\"."))
              unlink(tempFile, recursive = TRUE)
          }
)

test_that("getProfiles()",
          {
              tempFile <- tempfile()
              dir.create(tempFile)
              data(index)
              s <- expect_message(subset(index, 1:3), "Kept 3 profiles")
              p <- getProfiles(s, destdir=tempFile)
              expect <- expect_output(print(p), "argoFloats object of type \"profiles\" with 3 items")
              expect_equal(p[["cycle"]], c("124", "125", "126"))
              expect_equal(p[["length"]], 3)
              unlink(tempFile, recursive = TRUE)
          }
)

test_that("readProfiles()",
          {
              filename <- system.file("extdata", "D4900785_048.nc", package="argoFloats")
              argos <- expect_silent(readProfiles(filename))
          }
)

##> ## Comment this out because servers change, so we cannot predict success or failure
##> test_that("getProfile() with single out-of-date URL",
##>           {
##>               skip_if_not(hasArgoTestCache())
##>               data(index)
##>               s <- expect_message(subset(index, 778), "Kept 1 profiles")
##>               p <- expect_output(getProfiles(s, skip=FALSE), "|===")
##>               p <- expect_output(getProfiles(s, skip=TRUE), "|===")
##>               p <- expect_output(getProfiles(s),"|===") # default is skip=TRUE
##>           }
##> )

