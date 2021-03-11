## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
data("index")

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
              s <- expect_message(subset(index, 1:3), "Kept 3 cycles")
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

test_that("downloadWithRetries() can download files",
          {
              skip_if_offline()
              destDir <- tempfile()
              dir.create(destDir)
              urls <- rep("https://httpbin.org/get", 3)
              destFile <- c("file1", "file2", "file3")
              expect_identical(downloadWithRetries(urls, destDir, destFile, quiet = TRUE),
                               file.path(destDir, destFile))
              expect_true(all(file.exists(file.path(destDir, destFile))))
              unlink(destDir, recursive = TRUE)
          }
)

test_that("downloadWithRetries() can download zero files",
          {
              expect_identical(downloadWithRetries(character(), tempfile(), character()),
                               character())
          }
)

test_that("downloadWithRetries() skips downloads based on age",
          {
              destDir <- tempfile()
              dir.create(destDir)
              destFile <- file.path(destDir, "empty_file_that_exists.txt")
              expect_true(file.create(destFile))
              bogusUrlThatWillFail <- "this is not a url"
              
              expect_identical(downloadWithRetries(bogusUrlThatWillFail, destDir, basename(destFile),
                                                   age = Inf),
                               destFile)
              
              unlink(destDir, recursive = TRUE)
          }
)

test_that("downloadWithRetries() sets destination to NA on failure",
          {
              bogusUrlThatWillFail <- "this is not a url"
              
              expect_message(
                  expect_identical(
                      downloadWithRetries(bogusUrlThatWillFail, tempfile(), "dest", quiet = FALSE),
                      NA_character_
                  ),
                  "failed download"
              )
          }
)


test_that("downloadWithRetries() can download files with async=TRUE",
          {
              skip_if_offline()
              destDir <- tempfile()
              dir.create(destDir)
              urls <- rep("https://httpbin.org/get", 3)
              destFile <- c("file1", "file2", "file3")
              expect_identical(downloadWithRetries(urls, destDir, destFile, 
                                                   quiet = TRUE, async = TRUE),
                               file.path(destDir, destFile))
              expect_true(all(file.exists(file.path(destDir, destFile))))
              unlink(destDir, recursive = TRUE)
          }
)

test_that("downloadWithRetries() skips downloads based on age with async=TRUE",
          {
              destDir <- tempfile()
              dir.create(destDir)
              destFile <- file.path(destDir, "empty_file_that_exists.txt")
              expect_true(file.create(destFile))
              bogusUrlThatWillFail <- "this is not a url"
              
              expect_identical(downloadWithRetries(bogusUrlThatWillFail, destDir, basename(destFile),
                                                   age = Inf, async = TRUE),
                               destFile)
              
              unlink(destDir, recursive = TRUE)
          }
)

test_that("downloadWithRetries() sets destination to NA on failure with async=TRUE",
          {
              bogusUrlThatWillFail <- "this is not a url"
              
              expect_message(
                  expect_identical(
                      downloadWithRetries(bogusUrlThatWillFail, tempfile(), "dest", 
                                          quiet = FALSE, async = TRUE),
                      NA_character_
                  ),
                  "failed download"
              )
          }
)


##> ## Comment this out because servers change, so we cannot predict success or failure
##> test_that("getProfile() with single out-of-date URL",
##>           {
##>               skip_if_not(hasArgoTestCache())
##>               data(index)
##>               s <- expect_message(subset(index, 778), "Kept 1 cycles")
##>               p <- expect_output(getProfiles(s, skip=FALSE), "|===")
##>               p <- expect_output(getProfiles(s, skip=TRUE), "|===")
##>               p <- expect_output(getProfiles(s),"|===") # default is skip=TRUE
##>           }
##> )

