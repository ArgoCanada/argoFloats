## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

library(argoFloats)
library(testthat)

context("plot")

test_that("plot map", {
    data("index")
    expect_silent(plot(index, which="map", bathymetry=FALSE))
})

test_that("plot TS", {
    a <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")),
                        "Of 1 profiles read, 1 has")
    plot(a, which='TS')
})

test_that("plot qc", {
    a <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")),
                        "Of 1 profiles read, 1 has")
    expect_error(plot(a, which='qc', "In plot,argoFloats-method():
                      Please provide a parameter, one of pressure, pressureAdjusted, temperature,
                      temperatureAdjusted, salinity, salinityAdjusted, oxygen, oxygenAdjusted, chlorophyllA,
                      chlorophyllAAdjusted, BBP700, BBP700Adjusted, nitrate, nitrateAdjusted "))
    expect_error(plot(a, which='qc', parameter='doxy',
                      "In plot,argoFloats-method(): Parameter 'doxy' not found. Try one of: pressure,
                      pressureAdjusted, temperature, temperatureAdjusted, salinity, salinityAdjusted, oxygen, oxygenAdjusted,
                      chlorophyllA, chlorophyllAAdjusted, BBP700, BBP700Adjusted, nitrate, nitrateAdjusted"))
    plot(a, which='qc', parameter='oxygen')
}

    
          
          )