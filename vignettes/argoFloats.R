## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github("dankelley/oce", ref='develop')
#  devtools::install_github('dankelley/ocedata', ref='develop')
#  devtools::install_github('dankelley/argoFloats', ref='develop')

## ----eval=FALSE---------------------------------------------------------------
#  install.packages(devtools)

## ----workflow, echo=FALSE, fig.cap="Work flow of the argoFloats package.", fig.height=2.5, fig.width=7, fig.align='center', dev.args=list(pointsize=10)----
colText <- "darkblue"
colCode <- "black"
library(graphics)
textInBox <- function(x, y, text, cex=1, pos=4, center=TRUE, family="Times New Roman", col="black")
{
    w <- graphics::strwidth(text)
    h <- graphics::strheight(text)
    if (center)
        x <- x - w/2
    text(x, y, text, cex=cex, pos=pos, family=family, col=col)
    rect(x, y-h, x+1.1*w, y+h, border=col)
    invisible(list(w=w, h=h))
}
omar <- par("mar")
par(mar=c(0,1,0,0))
plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="", axes=FALSE)
x0 <- 0.25
y0 <- 0.9
dy <- 0.25
wh <- textInBox(x0, y0, "Get index from server", col=colText)
h <- wh$h * 1.2
xarrow <- x0 + 0.01 # not sure why we need to move it
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, "Get profile data files from server", col=colText)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, "Read profile data files", col=colText)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, "Process the data", col=colText)
x0 <- 0.8
y0 <- 0.9
dy <- 0.25
wh <- textInBox(x0, y0, "getIndex() ", family="sans", col=colCode)
h <- wh$h * 1.2
xarrow <- x0 + 0.01 # not sure why we need to move it
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1)
y0 <- y0 - dy
textInBox(x0, y0, "getProfiles() ", family="sans", col=colCode)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1)
y0 <- y0 - dy
textInBox(x0, y0, "readProfiles() ", family="sans", col=colCode)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1)
y0 <- y0 - dy
textInBox(x0, y0, " ...    ", family="sans", col=colCode)
par(mar=omar)

## ----message=FALSE------------------------------------------------------------
library(oce)
library(ocedata)
library(argoFloats)

## -----------------------------------------------------------------------------
data('indexMerged')

## -----------------------------------------------------------------------------
plot(indexMerged)

## ----eval=FALSE---------------------------------------------------------------
#  ai <- getIndex(file = "merge", destdir = "~/data/argo")

## ----echo=FALSE---------------------------------------------------------------
data(index)
ai <- index

## ---- message=FALSE-----------------------------------------------------------
# Subsetting by circle
Belon <- -64.7505 # Longitude of Bermuda
Belat <- 32.3078 # Latitude of Bermuda
aiBermuda <- subset(ai, circle=list(longitude=Belon, latitude=Belat, radius=100))
# Plotting circle subset
latbe <- aiBermuda[['latitude']]
lonbe <- aiBermuda[['longitude']]
belat <- 32
belon <- -64.5
data(coastlineWorldFine, package="ocedata")
plot(coastlineWorldFine, clongitude=belon, clatitude=belat, col="pink", span=400)
points(lonbe,latbe)

## ----fig.cap="Section plot made by oce package.", fig.height=4, fig.width=7, fig.align='center', dev.args=list(pointsize=10)----
data(section, package='oce')
plot(section)

## ----eval=FALSE---------------------------------------------------------------
#  sub <- subset(ai, 1:2) # To subset for profiles
#  profiles <- getProfiles(sub, destdir='~/data/argo')
#  argos <- readProfiles(profiles, handleFlags = TRUE)
#  pressure <- argos[['argos']][[1]][['pressure']]
#  temperature <- argos[['argos']][[1]][['temperature']]
#  plot(temperature, pressure, ylim=rev(range(pressure, na.rm=TRUE)), xlab='Temperature (C)', ylab='Depth (dbar)')

## ---- eval=FALSE--------------------------------------------------------------
#  #Installing necessary packages
#  library(devtools)
#  install_github('dankelley/argoFloats', ref='develop')
#  install_github("dankelley/oce", ref='develop')
#  install_github('dankelley/ocedata', ref='develop')
#  #Subsetting index by circle
#  ai <- getIndex(file='merge', destdir='~/data/argo')
#  Belon <- -64.7505 # Longitude of Bermuda
#  Belat <- 32.3078 # Latitude of Bermuda
#  aiBermuda <- subset(ai, circle=list(longitude=Belon, latitude=Belat, radius=100))
#  #Subsetting circle by time
#  from <- as.POSIXct("2010-01-01", tz="UTC")
#  to <- as.POSIXct("2018-10-31", tz="UTC")
#  ait <- subset(aiBermuda, time=list(from=from, to=to))
#  #Plotting new subset on a map
#  latbe <- ait[['latitude']]
#  lonbe <- ait[['longitude']]
#  belat <- 32
#  belon <- -64.5
#  data(coastlineWorldFine, package="ocedata")
#  plot(coastlineWorldFine, clongitude=belon, clatitude=belat, col="pink", span=400)
#  points(lonbe,latbe)

## ---- eval=FALSE--------------------------------------------------------------
#  library(argoFloats)
#  library(oce)
#  data(section, package='oce')
#  lat0 <- median(section[['latitude']])
#  lon0 <- median(section[['longitude']])
#  # Subset by rectangle
#  ai <- getIndex(file='merge', destdir='~/data/argo')
#  latRect <- lat0 + c(-2,2)
#  lonRect <- lon0 + c(-30,30)
#  air <- subset(ai, rectangle=list(longitude=lonRect, latitude=latRect))
#  # Subset the rectangle by time
#  from <- as.POSIXct("2017-01-01", tz="UTC")
#  to <- as.POSIXct("2017-12-31", tz="UTC")
#  aiTime <- subset(air, time=list(from=from, to=to))
#  #Plot this subset
#  latbe <- aiTime[['latitude']]
#  lonbe <- aiTime[['longitude']]
#  belat <- 37
#  belon <- -35
#  data(coastlineWorldFine, package="ocedata")
#  plot(coastlineWorldFine, clongitude=belon, clatitude=belat, col="pink", span=6000)
#  points(lonbe,latbe)

## ---- eval=FALSE--------------------------------------------------------------
#  ai <- getIndex(file='merge', destdir = '~/data/argo')
#  sub <- subset(ai, 1:2) # To subset for profiles
#  profiles <- getProfiles(sub, destdir='~/data/argo')
#  argos <- readProfiles(profiles, handleFlags = TRUE)
#  pressure <- argos[['argos']][[1]][['pressure']]
#  temperature <- argos[['argos']][[1]][['temperature']]
#  salinity <- argos[['argos']][[1]][['salinity']]
#  ctd<-as.ctd(salinity,temperature,pressure)
#  plotTS(ctd, eos="unesco")

