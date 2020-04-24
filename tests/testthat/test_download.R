## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)

context("bioargo download")

destdir <- "~/data/argo"

## FIXME(dek): add mswindows name for CR in next line (also, I was guessing on CR's linux name)
isDeveloper <- Sys.getenv("USER") == "kelley" || Sys.getenv("USERNAME") == "HARBINJ" || Sys.getenv("USER") == "richardsc"
canWrite <- file.exists(destdir) && file.info(destdir)$isdir
canTest <- isDeveloper && canWrite

test_that("getIndex() works with bioargo",
          {
              if (canTest) {
                  bai <- expect_silent(getIndex(file="argo_bio-profile_index.txt.gz", destdir=destdir))
              } else {
                  skip("only certain people can test this code\n")
              }
          }
)

