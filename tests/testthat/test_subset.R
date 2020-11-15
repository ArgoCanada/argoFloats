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

test_that("subset by float ID", {
          data("index")
          N <- 9
          indexID <- expect_message(subset(index, ID="1901584"),
                                    paste("Kept", N, "profiles"))
          expect_equal(dim(indexID[["index"]]), c(N, 8))
          expect_equal(indexID[["index"]][["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
          expect_equal(indexID[["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
})

test_that("subset by deep", {
          data("index")
          indexID <- expect_message(subset(index, deep=TRUE), "Kept 0 profiles \\(0%\\)")
})

test_that("silencing subset", {
          data("index")
          N <- 0
          indexID <- expect_error(subset(index, deep=TRUE, quiet=TRUE, " Error: in subset,argoFloats-method() :
  cannot give more than one method in the '...' argument"))
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
          index1 <- expect_error(subset(index, cycle=124, "Error: in subset,argoFloats-method() : \"cycle\" must be character value"))
          indexProfile <- expect_message(subset(index, cycle="124"), paste("Kept", N, "profiles"))
          expect_equal(dim(indexProfile[["index"]]), c(N, 8))
          expect_equal(indexProfile[["index"]][["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
          expect_equal(indexProfile[["file"]][1], "aoml/1901584/profiles/R1901584_124.nc")
})

test_that("subset by direction",
          {
              data(indexBgc)
              subset1 <- subset(indexBgc, direction='descent')
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
                  s <- expect_message(subset(i, ID="5902250"),
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
                  index1 <- expect_message(subset(index, ID="1901584"),
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

test_that("subset by dataMode",
          {
              data("index")
              N <- 517
              index1 <- expect_message(subset(index, dataMode="delayed"),
                                       paste("Kept", N, "profiles"))
              N <- 461
              index2 <- expect_message(subset(index, dataMode="realtime"),
                                       paste("Kept", N, "profiles"))
          }
)

test_that("subset by parameter", {
  data("indexBgc")
  N <- 37
  index1 <- expect_message(subset(indexBgc, parameter="DOXY"),
                           paste("Kept", N, "profiles"))
  index2 <- expect_error(subset(indexBgc, parameter="TEMP","Kept 0 profiles"))
                
  
})

test_that("subset stop messages", {
    data("index")
    N <- 9
    index1 <- expect_message(subset(index, ID="1901584"),
                             paste("Kept", N, "profiles"))
    argos <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")))
    argos2 <- expect_error(subset(argos, "Error: in subset,argoFloats-method() :
                                      must give 'column' or 'cycle' argument"))
    argos3 <- expect_error(subset(argos, column=2, 
    "Error: in subset,argoFloats-method() :
 cannot access column 2 of metadata item 'flags' because its dimension is 335 1 "))
    argos4 <- expect_message(subset(argos, cycle=131, "Kept 1 profiles (100%)"))
    argos5 <- expect_error(subset(argos, map=1, " Error: in subset,argoFloats-method():
  the only permitted '...' argument for argos type is 'column' or 'cycle'"))
    argos6 <- expect_error(subset(argos, cycle=1, "Error: In subset,argoFloats-method(): Cycle '1' not found. Try one of: 131"))
    index2 <- expect_error(subset(index, circle='dog', " Error: in subset,argoFloats-method() :
  'circle' must be a list containing 'longitude', 'latitude' and 'radius'"))
    index3 <- expect_error(subset(index, circle=list(longitude=-77.5, latitude=27.5), " Error: in subset,argoFloats-method() :
  'circle' must be a list containing 'longitude', 'latitude' and 'radius'"))
    index4 <- expect_error(subset(index, rectangle='dog', " Error: in subset,argoFloats-method():
  'rectangle' must be a list containing 'longitude' and 'latitude'"))
    index5 <- expect_error(subset(index, rectangle=list(longitude=c(-76.5, -76)), "Error: in subset,argoFloats-method():
  'rectangle' must be a list containing 'longitude' and 'latitude' "))
    index6 <- expect_error(subset(index, polygon='dog', " Error: in subset,argoFloats-method():
  'polygon' must be a list of two elements "))
    index7 <- expect_error(subset(index, polygon=list(c(1,3)), " Error: in subset,argoFloats-method():
  'polygon' must be a list of two elements "))
    index8 <- expect_error(subset(index, polygon=list(dog=c(1,2), cat=c(1,3)), "Error: in subset,argoFloats-method():
  'polygon' must be a list containing 'longitude' and 'latitude' "))
    index9 <- expect_error(subset(index, time=list(from="hi", to="bye"), " Error: in subset,argoFloats-method():
  'time' must be a list containing POSIX times "))
    index10 <- expect_error(subset(index, time=list(from=as.POSIXct("2019-12-31", tz="UTC"),to=as.POSIXct("2019-01-31", tz="UTC")),"Error: in subset,argoFloats-method():
 'to' must be greater than 'from' "))
    index11 <- expect_error(subset(index, dataMode=1, "Error: in subset,argoFloats-method():
  'dataMode' must be character value "))
    index12 <- expect_error(subset(index, dataMode='dog'," Error: in subset,argoFloats-method():
  'dataMode' must be either 'realtime' or 'delayed', not 'dog'"))
    index13 <- expect_error(subset(index, direction=1,"Error: in subset,argoFloats-method():
  'direction' must be character value of either 'ascent' or 'decent'"))
    index14 <- expect_error(subset(index,direction="dog", "Error: in subset,argoFloats-method():
  'direction' must be either 'ascent' or 'decent', not 'dog'" ))
    index15 <- expect_error(subset(index,parameter="temperature", " Error: there are no parameters for core Argo index objects. Try BGC, Merged, or Synthetic Argo. " ))
    
}
)

test_that("subset by dataStateIndicator", {
  if (canDownload()) {
  data("index")
  N <- 20
  index1 <- expect_message(subset(index, 1:20, paste("Kept", N, "profiles")))
  profiles <- expect_silent(getProfiles(index1))
  argos <- expect_output(expect_warning(readProfiles(profiles), "Of 20 profiles read, 8 have"), "|===")
  argos2 <- expect_silent(subset(argos, dataStateIndicator="2C"))
  expect_equal(11, argos2[["length"]])
  argos3 <- expect_silent(subset(argos, dataStateIndicator="J"))
  expect_equal(0, argos3[["length"]])
  }}
)
