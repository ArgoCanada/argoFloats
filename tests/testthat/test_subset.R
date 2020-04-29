## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)
library(testthat)

context("subset")

test_that("subset by circle", {
          data("index")
          a <- subset(index, circle=list(longitude=-77.06, latitude=26.54, radius=100))
          indexc <- expect_message(subset(index, circle=list(longitude=-77.06, latitude=26.54, radius=100)),
                                   "Kept 304 profiles \\(32%\\)")
          expect_equal(dim(indexc[["index"]]), c(304, 8))
          expect_equal(indexc[["index"]][["file"]][1], "aoml/4900183/profiles/D4900183_025.nc")
          expect_equal(indexc[["file"]][1], "aoml/4900183/profiles/D4900183_025.nc")
})

test_that("subset by rectangle", {
          data("index")
          indexr <- expect_message(subset(index, rectangle=list(longitude=c(-77, -76), latitude=c(25, 26))),
                                   "Kept 76 profiles \\(8%\\)")
          expect_equal(dim(indexr[["index"]]), c(76, 8))
          expect_equal(indexr[["index"]][["file"]][1], "aoml/4900183/profiles/D4900183_024.nc")
          expect_equal(indexr[["file"]][1], "aoml/4900183/profiles/D4900183_024.nc")
})

test_that("subset by polygon", {
          data("index")
          indexp <- expect_warning(expect_message(subset(index, polygon=list(latitude=c(25,27,25), longitude=c(-78,-77,-74))),
                                   "Kept 379 profiles \\(40%\\)"), "Closing the polygon, since the first and last points did not match.")
          expect_equal(dim(indexp[["index"]]), c(379,8))
          expect_equal(indexp[["index"]][["file"]][1], "aoml/3900582/profiles/D3900582_069.nc")
          expect_equal(indexp[["file"]][1], "aoml/3900582/profiles/D3900582_069.nc")
})

test_that("subset by time", {
          data("index")
          from <- as.POSIXct("2019-01-01", tz="UTC")
          to <- as.POSIXct("2019-12-31", tz="UTC")
          indext <- expect_message(subset(index, time=list(from=from, to=to)),
                                   "Kept 9 profiles \\(0.94%\\)")
          expect_equal(dim(indext[["index"]]), c(9,8))
          expect_equal(indext[["index"]][["file"]][1], "aoml/4901628/profiles/R4901628_212.nc")
          expect_equal(indext[["file"]][1], "aoml/4901628/profiles/R4901628_212.nc")
})

test_that("subset by institution", {
    data("index")
    indexi <- expect_message(subset(index, institution="AO"),
                             "Kept 897 profiles \\(94%\\)")
    expect_equal(dim(indexi[["index"]]), c(897,8))
    expect_equal(indexi[["index"]][["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
    expect_equal(indexi[["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
})

test_that("subset by float ID", {
    data("index")
    indexID <- expect_message(subset(index, ID="1901584"),
                             "Kept 9 profiles \\(0.94%\\)")
    expect_equal(dim(indexID[["index"]]), c(9,8))
    expect_equal(indexID[["index"]][["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
    expect_equal(indexID[["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
})

test_that("subset by deep", {
    data("index")
    indexID <- expect_message(subset(index, deep=TRUE), "Kept 0 profiles \\(0%\\)")
})

test_that("silencing subset", {
    data("index")
    indexID <- expect_silent(subset(index, deep=TRUE, silent=TRUE))
})
