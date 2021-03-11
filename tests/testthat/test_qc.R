## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(argoFloats)
context("quality-control flags")

##> ## DEK 2020-12-31:
##> ## I am commenting the next block out because it is not robust against changes to
##> ## data(index).
##> ## Perhaps this block can be uncommented and made robust later.
##>
##> test_that("applyQC with default 'flag' and 'action' arguments",
##>           {
##>               skip_if_not(hasArgoTestCache())
##>               data(index)
##>               i <- subset(index, 1:5) # first 5 profiles
##>               raw <- expect_output(expect_warning(readProfiles(getProfiles(i, argoDefaultDestdir())), "^Of 5 profiles read"), "|====")
##>               clean <- applyQC(raw)
##>               for (i in raw[["length"]]) {
##>                   for (field in c("temperature", "salinity", "pressure")) {
##>                       bad <- raw[["argos", i]][[paste0(field, "Flag")]] %in% c(0,3,4,6,7,9)
##>                       expect_true(all(is.na(clean[["argos", i]][[field]]) == bad))
##>                   }
##>               }
##>           }
##> )

##> ## DEK 2020-12-31:
##> ## I am commenting the next block out because it is not robust against changes to
##> ## data(index).
##> ## Perhaps this block can be uncommented and made robust later.
##>
##> test_that("showing QC tests", {
##>     skip_if_not(hasArgoTestCache())
##>     data('index')
##>     subset <- expect_message(subset(index, 1), "Kept 1 cycles")
##>     ## DK: this was not used
##>     ##indexq <- expect_message(subset(index, 1),
##>     ##                         "Kept 1 cycles \\(0.105%\\)")
##>     profiles <- getProfiles(subset)
##>     argos <- expect_output(expect_warning(readProfiles(profiles), "^Of 1 profiles read"), "|===")
##>     argos1 <- argos[[1]]
##>     ## this is only a consistency check with values as of 2020-07-17, not a real
##>     ## test of what to expect.
##>     expect_output(showQCTests(argos[[1]]), "^Tests performed: 2 3 4 5 6 13 14 15 16 17 19\\n    Failed test 14 \\(Density Inversion test\\)*$")
##> })
