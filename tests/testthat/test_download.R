## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)
source("can_download.R")
context("bgc download")
destdir <- "~/data/argo" # QUESTION: could we use a tmpdir and still be within CRAN guidelines?

test_that("getIndex()",
          {
              if (canDownload()) {
                  i <- expect_silent(getIndex(filename="argo_bio-profile_index.txt.gz", destdir=destdir))
                  i2 <- expect_error(getIndex(filename="dog","In get,argoFloats-method():
 filename type doesn't exist. Try one of these: \"argo\", \"core\", \"bgc\", \"bgcargo\", or \"synthetic\"."))
              }
          }
)

test_that("getProfiles()",
          {
              if (canDownload()) {
                  data(index)
                  s <- expect_message(subset(index, 1:3), "Kept 3 profiles \\(0.307%\\)")
                  p <- expect_silent(getProfiles(s))
                  expect_equal(p[["cycle"]], c("124", "125", "126"))
                  expect_equal(p[["file", 1]], "~/data/argo/R1901584_124.nc")
                  expect_equal(p[["file"]], c("~/data/argo/R1901584_124.nc", "~/data/argo/R1901584_125.nc",
                                              "~/data/argo/R1901584_126.nc"))
                  expect_equal(p[["file", 1:2]], c("~/data/argo/R1901584_124.nc", "~/data/argo/R1901584_125.nc"))
                  expect_equal(p[["length"]], 3)
              }
          }
)

test_that("readProfiles()",
          {
              if (canDownload()) {
                  data(index)
                  p <- expect_message(getProfiles(subset(index, 1:4)), "Kept 4 profiles \\(0.409%\\)")
                  a <- expect_output(expect_warning(readProfiles(p), "Of 4 profiles read, 4 have"), "|===")
                  expect_equal(4, length(a[["cycle"]]))
                  expect_true(is.character(a[["cycle"]]))
                  expect_true(inherits(a[[1]], "oce"))
                  expect_true(inherits(a[[1]], "argo"))
                  expect_equal(a[[1]][["longitude"]], -76.231)
                  expect_equal(a[[1]][["longitude"]], index[["longitude"]][1])
                  expect_equal(a[[1]][["latitude"]], 27.705)
                  expect_equal(a[[1]][["latitude"]], index[["latitude"]][1])
              }
          }
)

test_that("getProfile() handling of as single out-of-date URL",
          {
              if (canDownload()) {
                  data(index)
                  s <- expect_message(subset(index, 778), "Kept 1 profiles")
                  p <- expect_silent(getProfiles(s, skip=FALSE))
                  p <- expect_silent(getProfiles(s, skip=TRUE))
                  p <- expect_silent(getProfiles(s)) # default is skip=TRUE
              }
          }
)

test_that("readProfile() handling of an out-of-date URL surrounded by valid URLs",
          {
              if (canDownload()) {
                  data(index)
                  s <- expect_message(subset(index, 778 + seq(-1, 1)), "Kept 3 profiles")
                  p <- expect_silent(getProfiles(s)) # default is skip=TRUE
                  a <- expect_warning(expect_output(readProfiles(p), "|==="), "Of 3 profiles read, 1 has")
                  a <- expect_silent(readProfiles(p, quiet=TRUE))
              }
          }
)

test_that("readProfile() handling of nonlocal source file",
          {
              if (canDownload()) {
                  u <- "ftp://usgodae.org/pub/outgoing/argo/dac/aoml/5903586/profiles/BD5903586_001.nc"
                  if (packageVersion("oce") > "1.2.0") {
                      p <- expect_silent(readProfiles(u))
                  } else {
                      p <- expect_warning(readProfiles(u), "no variable named 'PRES_QC'")
                  }
                  expect_true(grepl("BD5903586_001.nc$", p[["filename"]]))
              }
          }
)
 
