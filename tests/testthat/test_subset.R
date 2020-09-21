## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)
##library(testthat)
source("can_download.R")               # need this if using getProfiles()

context("subset")

test_that("subset by circle",
          {
              N <- 310
              data("index")
              indexc <- expect_message(subset(index, circle=list(longitude=-77.06, latitude=26.54, radius=100)),
                                       paste("Kept", N, "profiles"))
              expect_equal(dim(indexc[["index"]]), c(N, 8))
              expect_equal(indexc[["index"]][["file"]][1], "aoml/4900183/profiles/D4900183_025.nc")
              expect_equal(indexc[["file"]][1], "aoml/4900183/profiles/D4900183_025.nc")
          }
)

test_that("subset by rectangle", {
          data("index")
          indexr <- expect_message(subset(index, rectangle=list(longitude=c(-77, -76), latitude=c(25, 26))),
                                   "Kept 76 profiles")
          expect_equal(dim(indexr[["index"]]), c(76, 8))
          expect_equal(indexr[["index"]][["file"]][1], "aoml/4900183/profiles/D4900183_024.nc")
          expect_equal(indexr[["file"]][1], "aoml/4900183/profiles/D4900183_024.nc")
})

test_that("subset by polygon", {
          data("index")
          N <- 382
          indexp <- expect_message(subset(index,
                                          polygon=list(latitude=c(25,27,25),
                                                       longitude=c(-78,-77,-74))),
                                   paste("Kept", N, "profiles"))
          expect_equal(dim(indexp[["index"]]), c(N, 8))
          expect_equal(indexp[["index"]][["file"]][1], "aoml/4901533/profiles/R4901533_080.nc")
          expect_equal(indexp[["file"]][1], "aoml/4901533/profiles/R4901533_080.nc")
})

test_that("subset by time", {
          data("index")
          N <- 9
          from <- as.POSIXct("2019-01-01", tz="UTC")
          to <- as.POSIXct("2019-12-31", tz="UTC")
          indext <- expect_message(subset(index, time=list(from=from, to=to)),
                                   paste("Kept", N, "profiles"))
          expect_equal(dim(indext[["index"]]), c(9,8))
          expect_equal(indext[["index"]][["file"]][1], "aoml/4901628/profiles/R4901628_212.nc")
          expect_equal(indext[["file"]][1], "aoml/4901628/profiles/R4901628_212.nc")
})

test_that("subset by institution", {
          data("index")
          N <- 914
          indexi <- expect_message(subset(index, institution="AO"),
                                   paste("Kept", N, "profiles"))
          expect_equal(dim(indexi[["index"]]), c(N, 8))
          expect_equal(indexi[["index"]][["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
          expect_equal(indexi[["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
})

test_that("subset by float id", {
          data("index")
          N <- 9
          indexid <- expect_message(subset(index, id="1901584"),
                                    paste("Kept", N, "profiles"))
          expect_equal(dim(indexid[["index"]]), c(N, 8))
          expect_equal(indexid[["index"]][["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
          expect_equal(indexid[["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
})

test_that("subset by deep", {
          data("index")
          indexid <- expect_message(subset(index, deep=TRUE), "Kept 0 profiles \\(0%\\)")
})

test_that("silencing subset", {
          data("index")
          N <- 0
          indexid <- expect_message(subset(index, deep=TRUE, quiet=TRUE),
                                    paste("Kept", N, "profiles"))
})

test_that("subset by ocean", {
          data("index")
          N <- 978
          indexOcean <- expect_message(subset(index, ocean='A'), paste("Kept", N, "profiles"))
          expect_equal(dim(indexOcean[["index"]]), c(N, 8))
          expect_equal(indexOcean[["index"]][["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
          expect_equal(indexOcean[["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
})

test_that("subset by cycle", {
          data("index")
          N <- 5
          indexProfile <- expect_message(subset(index, cycle=124), paste("Kept", N, "profiles"))
          expect_equal(dim(indexProfile[["index"]]), c(N, 8))
          expect_equal(indexProfile[["index"]][["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
          expect_equal(indexProfile[["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
})

test_that("subset by direction",
          {
              data(indexBgc)
              subset1 <- subset(indexBgc, direction='decent')
              subset2 <- subset(indexBgc, direction='ascent')
              expect_equal(subset1[["file"]], "coriolis/6901494/profiles/BD6901494_353D.nc")
              expect_equal(subset2[["file"]][1], "aoml/4900845/profiles/BR4900845_086.nc")
          }
)

test_that("subset by column",
          {
              if (canDownload()) {
                  i <- expect_silent(getIndex())
                  N <- 305
                  s <- expect_message(subset(i, id="5902250"),
                                      paste("Kept", N, "profiles"))
                  N <- 1
                  s <- expect_message(subset(s, cycle="253"),
                                      paste("Kept", N, "profiles"))
                  p <- expect_silent(getProfiles(s))
                  a <- expect_output(readProfiles(p), "|===")
                  a1 <- expect_silent(subset(a, column=1))
                  a2 <- expect_silent(subset(a, column=2))
                  salinity <- a[["salinity"]][[1]]
                  salinity1 <- a1[["salinity"]][[1]]
                  salinity2 <- a2[["salinity"]][[1]]
                  N <- 69
                  expect_equal(dim(salinity), c(N, 2))
                  expect_equal(dim(salinity1), c(N, 1))
                  expect_equal(dim(salinity2), c(N, 1))
                  expect_equal(salinity1, salinity[, 1, drop=FALSE])
                  expect_equal(salinity2, salinity[, 2, drop=FALSE])
              }
          }
)
test_that("subset by cycle",
          {
              if (canDownload()) {
                  data("index")
                  N <- 9
                  index1 <- expect_message(subset(index, id="1901584"),
                                           paste("Kept", N, "profiles"))
                  profiles <- expect_silent(getProfiles(index1))
                  argos <- expect_output(expect_warning(readProfiles(profiles),
                                          "Of 9 profiles read, 8 have"), "|===")
                  argos2 <- expect_message(subset(argos, cycle='147'),
                                           "Kept 1 profiles \\(11.1%\\)")
                  expect_equal(argos2[["cycle"]], "147")
                  expect_equal(unique(argos2[['cycle']]), "147")
              }
          }
)
