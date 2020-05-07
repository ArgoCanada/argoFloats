## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)

context("bioargo download")

destdir <- "~/data/argo"

## FIXME(dek): add mswindows name for CR in next line (also, I was guessing on CR's linux name)
isDeveloper <- Sys.getenv("USER") == "kelley" || Sys.getenv("USERNAME") == "HARBINJ" || Sys.getenv("USER") == "richardsc"
canWrite <- file.exists(destdir) && file.info(destdir)$isdir
canTest <- isDeveloper && canWrite

test_that("getIndex()",
          {
              if (canTest) {
                  i <- expect_silent(getIndex(filename="argo_bio-profile_index.txt.gz", destdir=destdir))
              } else {
                  skip("only certain people can test getIndex(), because it involves downloading\n")
              }
          }
)

test_that("getProfiles()",
          {
              if (canTest) {
                  data(index)
                  p <- expect_silent(getProfiles(subset(index, 1:3)))
                  expect_equal(p[["profile", 1]], "~/data/argo/R1901584_124.nc")
                  expect_equal(p[["profile"]], c("~/data/argo/R1901584_124.nc", "~/data/argo/R1901584_125.nc",
                                                 "~/data/argo/R1901584_126.nc"))
                  expect_equal(p[["profile", 1:2]], c("~/data/argo/R1901584_124.nc", "~/data/argo/R1901584_125.nc"))
                  expect_equal(p[["profile", "count"]], 3)
              } else {
                  skip("only certain people can test getProfiles(), because it involves downloading\n")
              }
          }
)

test_that("readProfiles()",
          {
              if (canTest) {
                  data(index)
                  p <- expect_silent(getProfiles(subset(index, 1:2)))
                  a <- expect_silent(readProfiles(p))
                  expect_equal(2, length(a[["profile"]]))
                  expect_true(is.list(a[["profile"]]))
                  expect_true(inherits(a[["profile",1]], "oce"))
                  expect_true(inherits(a[["profile",1]], "argo"))
              } else {
                  skip("only certain people can test readProfiles(), because it involves downloading\n")
              }
          }
)

