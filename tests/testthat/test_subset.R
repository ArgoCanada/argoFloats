## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

library(argoFloats)

context("subset")

test_that("subset by circle", {
          data("index")
          indexc <- expect_silent(subset(index, circle=list(longitude=-77.06, latitude=26.54, radius=100)))
          expect_equal(dim(indexc[["index"]]), c(304, 8))
})

test_that("subset by rectangle", {
          data("index")
          indexr <- expect_silent(subset(index,
                                         rectangle=list(longitude=c(-77, -76),
                                                        latitude=c(25, 26))))
          expect_equal(dim(indexr[["index"]]), c(76, 8))
          expect_equal(indexr[["index"]][["file"]][1], "aoml/4900183/profiles/D4900183_024.nc")
})

# Add test for polygon method here

# Add test for time method here

