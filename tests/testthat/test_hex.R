library(argoFloats)

context("hex to bits")

test_that("hex to bits", {
    expect_equal(hexToBits('2'), c(0, 1, 0, 0))
    expect_equal(hexToBits('e'), c(0, 1, 1, 1))
    expect_equal(hexToBits('4000'), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0))
})
