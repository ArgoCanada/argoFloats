## vim:textwidth=128:expandtab:shiftwidth=2:softtabstop=2
library(argoFloats)
context("useAdjusted() on core data data")

RMS <- function(x) sqrt(mean(x^2, na.rm=TRUE))

test_that("useAdjusted() on real-time file that holds Adjusted parameters", {
          a <- readProfiles(system.file("extdata", "R3901602_163.nc", package="argoFloats"), quiet=TRUE)
          # fallback="NA" case. First we check whether <PARAM> copied, then it's flags.
          aNA <- useAdjusted(a)
          for (param in c("pressure", "salinity", "temperature")) {
            expect_equal(a[[1]][[paste0(param,"Adjusted")]], aNA[[1]][[param]]) 
            expect_equal(a[[1]][[paste0(param,"AdjustedparamFlag")]], aNA[[1]][[paste0(param,"paramFlag")]]) 
          }
          # fallback="raw" case
          araw <- useAdjusted(a, fallback=TRUE)
          for (param in c("pressure", "salinity", "temperature")) {
            expect_equal(a[[1]][[paste0(param,"Adjusted")]], aNA[[1]][[param]]) 
            expect_equal(a[[1]][[paste0(param,"AdjustedparamFlag")]], aNA[[1]][[paste0(param,"paramFlag")]]) 
          }
})

test_that("useAdjusted() on real-time file that holds Adjusted parameters (S faked to NA)", {
          a <- readProfiles(system.file("extdata", "R3901602_163.nc", package="argoFloats"), quiet=TRUE)
          # FAKE: set all salinityAdjusted to NA to test fallback="raw"
          fake <- a
          fake@data$argos[[1]][["salinityAdjusted"]] <- matrix(NA,
                                                               nrow=nrow(fake@data$argos[[1]][["salinity"]]),
                                                               ncol=ncol(fake@data$argos[[1]][["salinity"]]))
          # fallback="NA" case. First we check whether <PARAM> copied, then it's flags.
          fakeNA <- useAdjusted(fake)
          expect_true(all(is.na(fakeNA[[1]][["salinity"]])))
          for (param in c("pressure", "temperature"))
            expect_equal(fakeNA[[1]][[param]], fake[[1]][[paste0(param, "Adjusted")]])
          # fallback="raw" case
          fakeraw <- useAdjusted(fake, fallback=TRUE)
          for (param in c("pressure", "temperature"))
            expect_equal(fakeraw[[1]][[param]], fake[[1]][[paste0(param, "Adjusted")]])
})

test_that("useAdjusted() on delayed-mode file data", {
          a <- readProfiles(system.file("extdata", "D4900785_048.nc", package="argoFloats"), quiet=TRUE)
          # Check against a known value.  (If the RMS diff were zero, the test wouldn't tell much.)
          expect_equal(0.002093299, RMS(a[[1]][["salinity"]] - a[[1]][["salinityAdjusted"]]))
          # fallback="NA" case. First we check whether <PARAM> copied, then it's flags.
          aNA <- useAdjusted(a)
          for (param in c("pressure", "salinity", "temperature")) {
            expect_equal(a[[1]][[paste0(param,"Adjusted")]], aNA[[1]][[param]]) 
            expect_equal(a[[1]][[paste0(param,"AdjustedparamFlag")]], aNA[[1]][[paste0(param,"paramFlag")]]) 
          }
          # fallback="raw" case
          araw <- useAdjusted(a, fallback=TRUE)
          for (param in c("pressure", "salinity", "temperature")) {
            expect_equal(a[[1]][[paste0(param,"Adjusted")]], araw[[1]][[param]]) 
            expect_equal(a[[1]][[paste0(param,"AdjustedparamFlag")]], araw[[1]][[paste0(param,"paramFlag")]]) 
          }
})

test_that("useAdjusted() on FAKED adjusted-mode data", {
          a <- readProfiles(system.file("extdata", "D4900785_048.nc", package="argoFloats"), quiet=TRUE)
          # FAKE an 'adjusted' mode case
          a@data$argos[[1]]@metadata$dataMode <- "A"
          # Check against a known value.  (If the RMS diff were zero, the test wouldn't tell much.)
          expect_equal(0.002093299, RMS(a[[1]][["salinity"]] - a[[1]][["salinityAdjusted"]]))
          # fallback="NA" case. First we check whether <PARAM> copied, then it's flags.
          aNA <- useAdjusted(a)
          for (param in c("pressure", "salinity", "temperature")) {
            expect_equal(a[[1]][[paste0(param,"Adjusted")]], aNA[[1]][[param]]) 
            expect_equal(a[[1]][[paste0(param,"AdjustedparamFlag")]], aNA[[1]][[paste0(param,"paramFlag")]]) 
          }
          # fallback="raw" case
          araw <- useAdjusted(a, fallback=TRUE)
          for (param in c("pressure", "salinity", "temperature")) {
            expect_equal(a[[1]][[paste0(param,"Adjusted")]], araw[[1]][[param]]) 
            expect_equal(a[[1]][[paste0(param,"AdjustedparamFlag")]], araw[[1]][[paste0(param,"paramFlag")]]) 
          }
})

