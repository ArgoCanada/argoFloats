## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
## The tests relating to data dimensions will need to be altered if the dataset is altered.

## Next is used in some old tests (not yet updated) that required that all the
## entries in data(index) would stay valid forever.

library(argoFloats)
context("subset")

data("index")
data("indexBgc")

time <- index[["time"]]
cycle <- index[["cycle"]]
ocean <- index[["ocean"]]
ID <- index[["ID"]]
lon <- index[["longitude"]]
lat <- index[["latitude"]]

test_that("subset by circle", {
          D <- oce::geodDist(lon, lat, -77.06, 26.54)
          N <- sum(D<=100)
          indexc <- expect_message(subset(index, circle=list(longitude=-77.06, latitude=26.54, radius=100)),
                                   paste("Kept", N, "cycles"))
})

test_that("subset by rectangle", {
          N <- sum(-77 <= lon & lon <= -76 & 25 <= lat & lat <= 26)
          indexr <- expect_message(subset(index, rectangle=list(longitude=c(-77, -76), latitude=c(25, 26))),
                                   paste("Kept", N, "cycles"))
})

test_that("subset by polygon", {
          Polygon <- sf::st_polygon(list(outer=cbind(c(-78,-77,-74,-78), c(25,27,25,25))))
          Points <- sf::st_multipoint(cbind(lon, lat))
          intersection <- sf::st_intersection(Points, Polygon)
          N <- nrow(intersection)
          expect_message(subset(index, polygon=list(latitude=c(25,27,25), longitude=c(-78,-77,-74))),
                         paste("Kept", N, "cycles"))
})

test_that("subset by time", {
          from <- as.POSIXct("2019-01-01", tz="UTC")
          to <- as.POSIXct("2019-12-31", tz="UTC")
          N <- sum(from <= time & time <= to)
          indext <- expect_message(subset(index, time=list(from=from, to=to)),
                                   paste("Kept", N, "cycles"))
})

test_that("subset by institution", {
          N <- sum(index[["institution"]] == "AO")
          indexi <- expect_message(subset(index, institution="AO"),
                                   paste("Kept", N, "cycles"))
})

test_that("subset by float ID", {
          N <- sum(ID == "1901584")
          indexID <- expect_message(subset(index, ID="1901584"),
                                    paste("Kept", N, "cycles"))
})

test_that("subset by deep", {
          indexID <- expect_message(subset(index, deep=TRUE), "Kept 0 cycles \\(0%\\)")
})

test_that("silencing subset", {
          N <- 0
          indexID <- expect_error(subset(index, deep=TRUE, quiet=TRUE, " Error: in subset,argoFloats-method() : cannot give more than one method in the '...' argument"))
})

test_that("subset by ocean", {
          N <- sum(ocean == "A")
          indexOcean <- expect_message(subset(index, ocean='A'), paste("Kept", N, "cycles"))
})

test_that("subset by cycle", {
          N <- sum(cycle == 124)
          expect_message(subset(index, cycle=124), paste("Kept", N, "cycles"))
          N <- sum(cycle %in% 124:125)
          index1 <- expect_message(subset(index, cycle=124:125), paste("Kept", N, "cycles"))
})

test_that("subset by profile", {
    skip_if_not(hasArgoTestCache())
    i <- getIndex(quiet=TRUE)
    N <- sum(i[["ID"]] == "5902250")
    s <- expect_message(subset(i, ID="5902250"), paste("Kept", N, "cycles"))
    N <- 1
    s <- expect_message(subset(s, cycle="253"), paste("Kept", N, "cycles"))
    p <- getProfiles(s)
    a <- expect_output(readProfiles(p), "|===") # Robustness: OK if float stays in archive
    a1 <- expect_silent(subset(a, profile=1))
    a2 <- expect_silent(subset(a, profile=2))
    salinity <- a[["salinity"]][[1]]
    salinity1 <- a1[["salinity"]][[1]]
    salinity2 <- a2[["salinity"]][[1]]
    ## Reach inside the oce::argo object to get N.  Was hard-wired
    ## before, and likely that's okay, but I wanted to check. -- DEK
    N <- nrow(a@data$argos[[1]]@data$pressure)
    expect_equal(dim(salinity), c(N, 2))
    expect_equal(dim(salinity1), c(N, 1))
    expect_equal(dim(salinity2), c(N, 1))
    expect_equal(salinity1, salinity[, 1, drop=FALSE])
    expect_equal(salinity2, salinity[, 2, drop=FALSE])
})

##> ## DEK 2020-12-31:
##> ## I am commenting the next block out because it is not robust against changes to
##> ## data(index). Pluse, we have a check on subset-by-cycle above, so do we need this?
##> ## Perhaps this block can be uncommented and made robust later.
##>
##> test_that("subset by cycle", {
##>           skip_if_not(hasArgoTestCache())
##>           data("index")
##>           N <- 9
##>           index1 <- expect_message(subset(index, ID="1901584"),
##>                                    paste("Kept", N, "cycles"))
##>           profiles <- expect_output(getProfiles(index1),"|===")
##>           argos <- expect_output(expect_warning(readProfiles(profiles),
##>                                                 "Of 9 profiles read, 8 have"), "|===")
##>           argos2 <- expect_message(subset(argos, cycle='147'),
##>                                    "Kept 1 cycles \\(11.1%\\)")
##>           expect_equal(argos2[["cycle"]], "147")
##>           expect_equal(unique(argos2[['cycle']]), "147")
##> })

test_that("subset by dataMode", {
          Ndelayed <- sum(grepl(".*D[0-9_abc]+.nc$", index[["file"]]))
          index1 <- expect_message(subset(index, dataMode="delayed"),
                                   paste("Kept", Ndelayed, "cycles"))
          Nrealtime <- sum(grepl(".*R[0-9_abc]+.nc$", index[["file"]]))
          index2 <- expect_message(subset(index, dataMode="realtime"),
                                   paste("Kept", Nrealtime, "cycles"))
})

test_that("subset by parameter", {
          N <- sum(grepl("DOXY", indexBgc[["parameters"]]))
          index1 <- expect_message(subset(indexBgc, parameter="DOXY"),
                                   paste("Kept", N, "cycles"))
})

test_that("subset stop messages", {
          argos <- expect_warning(readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats")))
          expect_error(subset(argos, "Error: in subset,argoFloats-method() : must give 'profile' or 'cycle' argument"))
          expect_error(subset(argos, profile=2,
                              "Error: in subset,argoFloats-method() : cannot access profile 2 of metadata item 'flags' because its dimension is 335 1 "))
          argos2 <- expect_message(subset(argos, cycle=131), "Kept 1 cycles")
          expect_error(subset(argos, map=1, " Error: in subset,argoFloats-method(): the only permitted '...' argument for argos type is 'profile' or 'cycle'"))
          expect_error(subset(argos, cycle="1", "Error: In subset,argoFloats-method(): Cycle '1' not found. Try one of: 131"))
          expect_error(subset(index, circle='dog', " Error: in subset,argoFloats-method() : 'circle' must be a list containing 'longitude', 'latitude' and 'radius'"))
          expect_error(subset(index, circle=list(longitude=-77.5, latitude=27.5), " Error: in subset,argoFloats-method() : 'circle' must be a list containing 'longitude', 'latitude' and 'radius'"))
          expect_error(subset(index, rectangle='dog', " Error: in subset,argoFloats-method(): 'rectangle' must be a list containing 'longitude' and 'latitude'"))
          expect_error(subset(index, rectangle=list(longitude=c(-76.5, -76)), "Error: in subset,argoFloats-method(): 'rectangle' must be a list containing 'longitude' and 'latitude' "))
          expect_error(subset(index, polygon='dog', " Error: in subset,argoFloats-method(): 'polygon' must be a list of two elements "))
          expect_error(subset(index, polygon=list(c(1,3)), " Error: in subset,argoFloats-method(): 'polygon' must be a list of two elements "))
          expect_error(subset(index, polygon=list(dog=c(1,2), cat=c(1,3)), "Error: in subset,argoFloats-method(): 'polygon' must be a list containing 'longitude' and 'latitude' "))
          expect_error(subset(index, time=list(from="hi", to="bye"), " Error: in subset,argoFloats-method(): 'time' must be a list containing POSIX times "))
          expect_error(subset(index, time=list(from=as.POSIXct("2019-12-31", tz="UTC"),to=as.POSIXct("2019-01-31", tz="UTC")),"Error: in subset,argoFloats-method(): 'to' must be greater than 'from' "))
          expect_error(subset(index, dataMode=1, "Error: in subset,argoFloats-method(): 'dataMode' must be character value "))
          expect_error(subset(index, dataMode='dog'," Error: in subset,argoFloats-method(): 'dataMode' must be either 'realtime' or 'delayed', not 'dog'"))
          expect_error(subset(index, direction=1,"Error: in subset,argoFloats-method(): 'direction' must be character value of either 'ascent' or 'decent'"))
          expect_error(subset(index,direction="dog", "Error: in subset,argoFloats-method(): 'direction' must be either 'ascent' or 'decent', not 'dog'" ))
          expect_error(subset(index,parameter="temperature", " Error: there are no parameters for core Argo index objects. Try BGC, Merged, or Synthetic Argo. " ))

})


##> ## DEK 2020-12-31:
##> ## I am commenting the next block out because it is not robust against changes to
##> ## data(index).
##> ## Perhaps this block can be uncommented and made robust later.
##>
##> test_that("subset by dataStateIndicator", {
##>           skip_if_not(robustAgainstIndexChanges)
##>           skip_if_not(hasArgoTestCache())
##>           data("index")
##>           N <- 20
##>           index1 <- expect_message(subset(index, 1:20, paste("Kept", N, "cycles")))
##>           profiles <- expect_output(getProfiles(index1),"|===")
##>           argos <- expect_output(expect_warning(readProfiles(profiles), "Of 20 profiles read, 8 have"), "|===")
##>           argos2 <- expect_silent(subset(argos, dataStateIndicator="2C"))
##>           expect_equal(11, argos2[["length"]])
##>           argos3 <- expect_silent(subset(argos, dataStateIndicator="J"))
##>           expect_equal(0, argos3[["length"]])
##> })
