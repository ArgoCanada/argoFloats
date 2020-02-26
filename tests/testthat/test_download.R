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


test_that("subset() works with small circle (should give warning)",
          {
              if (canTest) {
                  bai <- expect_silent(getIndex(file="argo_bio-profile_index.txt.gz", destdir=destdir))
                  ## Should be none near Winnipeg, Canada
                  s1 <- expect_warning(subset(bai, circle=list(longitude=-97.2, latitude=49.9, radius=1)), "found no profiles")
                  ## Should be several near Sable Island
                  s2 <- expect_silent(subset(bai, circle=list(longitude=-59.915, latitude=44.934, radius=200)))
              } else {
                  skip("only certain people can test this code\n")
              }
          }
)


## FIXME(dek): more tests here, on getProfiles() and contents of those profiles. Look in ?getProfiles for a starting point.

