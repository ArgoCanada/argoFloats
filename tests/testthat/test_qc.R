## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)
source("can_download.R")
context("processing of QC flags")

test_that("applyQC with default 'flag' and 'action' arguments",
          {
              if (canDownload()) {
                  data(index)
                  i <- subset(index, 1:5) # first 5 profiles
                  raw <- expect_warning(readProfiles(getProfiles(i)), "^Of 5 profiles read")
                  clean <- applyQC(raw)
                  for (i in raw[["length"]]) {
                      for (field in c("temperature", "salinity", "pressure")) {
                          bad <- raw[["profile", i]][[paste0(field, "Flag")]] %in% c(0,3,4,6,7,9)
                          expect_true(all(is.na(clean[["profile", i]][[field]]) == bad))
                      }
                  }
              }
          }
)
