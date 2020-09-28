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

test_that("plot QC", {
    a <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")),
                        "Of 1 profiles read, 1 has")
    expect_error(plot(a, which='QC', "In plot,argoFloats-method():
                      Please provide a parameter, one of pressure, pressureAdjusted, temperature,
                      temperatureAdjusted, salinity, salinityAdjusted, oxygen, oxygenAdjusted, chlorophyllA,
                      chlorophyllAAdjusted, BBP700, BBP700Adjusted, nitrate, nitrateAdjusted "))
    expect_error(plot(a, which='QC', parameter='doxy',
                      "In plot,argoFloats-method(): Parameter 'doxy' not found. Try one of: pressure,
                      pressureAdjusted, temperature, temperatureAdjusted, salinity, salinityAdjusted, oxygen, oxygenAdjusted,
                      chlorophyllA, chlorophyllAAdjusted, BBP700, BBP700Adjusted, nitrate, nitrateAdjusted"))
    plot(a, which='QC', parameter='oxygen')
}



          )

test_that("error messages", {
    data("index")
    a <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")),
                        "Of 1 profiles read, 1 has")
    expect_error(plot(argos, which='TS', eos='dog', " Error: In plot,argoFloats-method():
 eos must be \"gsw\" or \"unesco\", not \"dog\" "))
    expect_error(plot(index, which='TS', " Error: In plot,argoFloats-method(): : x must have been created by readProfiles()"))
    expect_error(plot(index, which="QC", " Error: In plot,argoFloats-method(): The type of x must be 'argos' "))
    expect_error(plot(index, which='dog', "Error: In plot,argoFloats-method():cannot handle which=\"dog\"; see ?'plot,argoFloats-method' "))



})
