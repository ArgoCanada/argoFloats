## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)
source("can_download.R")
context("quality-control flags")

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
