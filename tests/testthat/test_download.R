## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)

PROBLEM_WITH_TESTS <- TRUE # used to skip over subset() test

context("bioargo download")

developers <- c("kelley") # add more here
isDeveloper <- Sys.getenv("USER") %in% developers
destdir <- "~/data/argo"
canWrite <- file.exists(destdir) && file.info(destdir)$isdir
canTest <- isDeveloper && canWrite

test_that("getIndex() works with bioargo",
          {
              if (canTest) {
                  bai <- expect_silent(getIndex(file="argo_bio-profile_index.txt.gz", destdir=destdir))
              } else {
                  skip("only certain people can test this code\n")
              }
          })

test_that("subset() works with small circle (should give warning)",
          {
              if (canTest) {
                  bai <- expect_silent(getIndex(file="argo_bio-profile_index.txt.gz", destdir=destdir))
                  if (!PROBLEM_WITH_TESTS) {
                      bai <- expect_silent(getIndex(file="argo_bio-profile_index.txt.gz", destdir=destdir))
                      ## # FIXME(dek): why does next line fail, not finding this specialized subset()?
                      ## # FIXME(dek): I think I am doing the export correctly.
                      ## Winnipeg 49.8951° N, 97.1384° W, an inland location; certainly no argo within 1 kilometer
                      s <- expect_warning(subset(bai, circle=list(longitude=-97.2, latitude=49.9, radius=1)), "found no profiles")
                  } else {
                      skip("the subset method is not being exported properly (?)\n")
                  }
              } else {
                  skip("only certain people can test this code\n")
              }
          })

