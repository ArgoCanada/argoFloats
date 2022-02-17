## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

context("plot")

test_that("plot map", {
    data(index)
    data(topoWorld, package="oce")
    expect_silent(plot(index, bathymetry=list(source=topoWorld, contour=TRUE)))
    expect_silent(plot(index, bathymetry=list(source=topoWorld)))
})

test_that("plot TS", {
    a <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")), "Of 1 profiles read, 1 has")
    plot(a, which="TS")
})

test_that("plot QC", {
    a <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")),
        "Of 1 profiles read, 1 has")
    expect_silent(plot(a, which="QC"))
    expect_silent(plot(a, which="QC", QCControl=list(parameter="oxygen")))
})

test_that("error messages", {
    data("index")
    a <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")),
        "Of 1 profiles read, 1 has")
    expect_error(plot(a, which="TS", eos="dog"), "eos must be")
    expect_error(plot(index, which="TS"), "x must have been created by readProfiles()")
    expect_error(plot(index, which="QC"), "type of x must be .argos.")
    expect_error(plot(index, which="dog"), "cannot handle which=\"dog\"")
})

test_that("plot profile", {
    a <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")),
        "Of 1 profiles read, 1 has")
    expect_silent(plot(a, which="profile"))
    expect_error(plot(a, which="profile", profileControl=list(parameter="nitrate")),
        "In plot,argoFloats-method()") # no test on details because regexp has tricky quotes, $ etc
})

