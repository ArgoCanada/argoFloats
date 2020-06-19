library(argoFloats)
library(testthat)

context("hex to nibble")

test_that("hex to nibble", {
    
    expect_equal(hexToNibble('2'), c(0, 0, 1, 0))
})

test_that("hex to nibble", {
    
    expect_equal(hexToNibble('e'), c(1, 1, 1, 0))
})