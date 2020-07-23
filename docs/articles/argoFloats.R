## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment="#>")

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github("ArgoCanada/oce", ref='develop')
#  devtools::install_github('ArgoCanada/ocedata', ref='develop')
#  devtools::install_github('ArgoCanada/argoFloats', ref='develop')

## ----eval=FALSE---------------------------------------------------------------
#  install.packages(devtools)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(oce)
library(ocedata)
library(argoFloats)

## -----------------------------------------------------------------------------
data('index')

## ---- fig.cap="Figure 2: Built in index demonstrating argo floats within 200 km of Bahamas."----
plot(index, bathymetry=FALSE)          # also, try using bathymetry=TRUE

## ---- eval=FALSE--------------------------------------------------------------
#  index

## ----eval=FALSE---------------------------------------------------------------
#  ai <- getIndex("argo")

## ----echo=FALSE---------------------------------------------------------------
data(index)
ai <- index

## ---- message=FALSE, fig.cap="Figure 3: 50 km radius and polygon subset of argo floats found off the coast of Bahamas", warning=FALSE----
# Subsetting by circle
aiCircle <- subset(ai, circle=list(longitude=-77.5, latitude=27.5, radius=50))
# Subsetting by polygon
lonPoly <- c(-76.5, -76.0, -75.5)
latPoly <- c(25.5, 26.5, 25.5)
aiPoly <- subset(ai, polygon=list(longitude=lonPoly, latitude=latPoly))
# Plotting the subsets together
CP <- merge(aiCircle, aiPoly)
plot(CP, bathymetry=FALSE)             # also, try using bathymetry=TRUE

## ----fig.cap="Figure 4: Line A03 section plot made by oce package.", fig.height=4, fig.width=7, fig.align='center', dev.args=list(pointsize=10)----
data(section, package='oce')
plot(section)

## ----eval=FALSE---------------------------------------------------------------
#  sub <- subset(ai, 1:2) # To subset for profiles
#  profiles <- getProfiles(sub)
#  argos <- readProfiles(profiles)
#  argosClean <- applyQC(argos)
#  pressure <- argosClean[['argos']][[1]][['pressure']]
#  temperature <- argosClean[['argos']][[1]][['temperature']]
#  plot(temperature, pressure, ylim=rev(range(pressure, na.rm=TRUE)),
#       xlab='Temperature (C)', ylab='Depth (dbar)')

## ---- eval=FALSE--------------------------------------------------------------
#  library(argoFloats)
#  ai <- getIndex("argo")
#  # Subsetting by circle
#  aiCircle <- subset(ai, circle=list(longitude=-77.5, latitude=27.5, radius=50))
#  # Subsetting by polygon
#  lonPoly <- c(-76.5, -76.0, -75.5)
#  latPoly <- c(25.5, 26.5, 25.5)
#  aiPoly <- subset(ai, polygon=list(longitude=lonPoly, latitude=latPoly))
#  #Subset by time
#  from <- as.POSIXct("2013-01-01", tz="UTC")
#  to <- as.POSIXct("2013-12-31", tz="UTC")
#  aic <- subset(aiCircle, time=list(from=from, to=to))
#  aip <- subset(aiPoly, time=list(from=from, to=to))
#  # Plotting the subsets together
#  cp <- merge(aic, aip)
#  plot(cp, bathymetry=FALSE)             # also, try using bathymetry=TRUE

## ---- eval=FALSE--------------------------------------------------------------
#  library(argoFloats)
#  library(oce)
#  data(section, package='oce')
#  lat0 <- median(section[['latitude']])
#  lon0 <- median(section[['longitude']])
#  # Subset by rectangle
#  ai <- getIndex("argo")
#  latRect <- lat0 + c(-2,2)
#  lonRect <- lon0 + c(-30,30)
#  air <- subset(ai, rectangle=list(longitude=lonRect, latitude=latRect))
#  # Subset the rectangle by time
#  from <- as.POSIXct("2017-01-01", tz="UTC")
#  to <- as.POSIXct("2017-12-31", tz="UTC")
#  aiTime <- subset(air, time=list(from=from, to=to))
#  #Plot this subset
#  plot(aiTime, bathymetry=FALSE)         # also, try using bathymetry=TRUE

## ---- eval=FALSE--------------------------------------------------------------
#  library(argoFloats)
#  ai <- getIndex("merge")
#  sub <- subset(ai, 1:2) # To subset for profiles
#  profiles <- getProfiles(sub)
#  argos <- readProfiles(profiles)
#  argosClean <- applyQC(argos)
#  plot(argosClean, which='TS')

