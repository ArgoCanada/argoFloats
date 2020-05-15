## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)
source("can_download.R")
context("bioargo download")
destdir <- "~/data/argo" # QUESTION: could we use a tmpdir and still be within CRAN guidelines?

test_that("getIndex()",
          {
              if (canDownload(destdir)) {
                  i <- expect_silent(getIndex(filename="argo_bio-profile_index.txt.gz", destdir=destdir))
              } else {
                  skip("only certain people can test getIndex(), because it involves downloading\n")
              }
          }
)

test_that("getProfiles()",
          {
              if (canDownload(destdir)) {
                  data(index)
                  s <- expect_message(subset(index, 1:3), "Kept 3 profiles \\(0.31%\\)")
                  p <- expect_silent(getProfiles(s))
                  expect_equal(p[["file", 1]], "~/data/argo/R1901584_124.nc")
                  expect_equal(p[["file"]], c("~/data/argo/R1901584_124.nc", "~/data/argo/R1901584_125.nc",
                                              "~/data/argo/R1901584_126.nc"))
                  expect_equal(p[["file", 1:2]], c("~/data/argo/R1901584_124.nc", "~/data/argo/R1901584_125.nc"))
                  expect_equal(p[["length"]], 3)
              } else {
                  skip("only certain people can test getProfiles(), because it involves downloading\n")
              }
          }
)

test_that("readProfiles()",
          {
              if (canDownload(destdir)) {
                  data(index)
                  p <- expect_message(getProfiles(subset(index, 1:2)), "Kept 2 profiles \\(0.21%\\)")
                  a <- expect_silent(readProfiles(p))
                  expect_equal(2, length(a[["profile"]]))
                  expect_true(is.list(a[["profile"]]))
                  expect_true(inherits(a[["profile",1]], "oce"))
                  expect_true(inherits(a[["profile",1]], "argo"))
                  expect_equal(a[["profile",1]][["longitude"]], -76.231)
                  expect_equal(a[["profile",1]][["longitude"]], index[["longitude"]][1])
                  expect_equal(a[["profile",1]][["latitude"]], 27.705)
                  expect_equal(a[["profile",1]][["latitude"]], index[["latitude"]][1])
              } else {
                  skip("only certain people can test readProfiles(), because it involves downloading\n")
              }
          }
)

