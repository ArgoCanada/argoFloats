## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github("dankelley/oce", ref='develop')
#  devtools::install_github('dankelley/ocedata', ref='develop')
#  devtools::install_github('dankelley/argoFloats', ref='develop')

## ----eval=FALSE---------------------------------------------------------------
#  install.packages(devtools)

## ----workflow, echo=FALSE, fig.cap="Figure 1: Work flow of the argoFloats package.", fig.height=2.8, fig.width=5, fig.align='center', dev.args=list(pointsize=10), warning=FALSE----
colText <- "darkblue"
colCode <- "black"
library(graphics)
textInBox <- function(x, y, text, cex=1, pos=4, center=TRUE, family="Times New Roman", col="black", tweakx=0)
{
    w <- graphics::strwidth(text)
    h <- graphics::strheight(text)
    if (center)
        x <- x - w/2
    text(x+tweakx, y, text, cex=cex, pos=pos, family=family, col=col)
    rect(x+tweakx, y-h, x+tweakx+1.1*w, y+h, border=col)
    invisible(list(w=w, h=h))
}
omar <- par("mar")
par(mar=c(0,1,0,0))
plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="", axes=FALSE)
x0 <- 0.25
y0 <- 0.9
dy <- 0.2
wh <- textInBox(x0, y0, "Get index from server", col=colText)
h <- wh$h * 1.2
xarrow <- x0 + 0.01 # not sure why we need to move it
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, "Focus on a subset of profiles", col=colText)
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
wh <- textInBox(x0, y0, "getIndex() ", family="sans", col=colCode)
h <- wh$h * 1.2
xarrow <- x0 + 0.01 # not sure why we need to move it
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1)
y0 <- y0 - dy
textInBox(x0, y0, "subset() ", family="sans", col=colCode)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1)
y0 <- y0 - dy
textInBox(x0, y0, "getProfiles() ", family="sans", col=colCode)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1)
y0 <- y0 - dy
textInBox(x0, y0, "readProfiles() ", family="sans", col=colCode)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1)
y0 <- y0 - dy
textInBox(x0, y0, ". . .   ", family="sans", col=colCode, tweakx=0.005)
par(mar=omar)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(oce)
library(ocedata)
library(argoFloats)

## -----------------------------------------------------------------------------
data('index')

## ---- fig.cap="Figure 2: Built in index demonstrating argo floats within 200 km of Bahamas"----
plot(index)

## ----eval=FALSE---------------------------------------------------------------
#  ai <- getIndex(file = "argo", destdir = "~/data/argo")

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
plot(CP)

## ----fig.cap="Figure 4: Line A03 section plot made by oce package.", fig.height=4, fig.width=7, fig.align='center', dev.args=list(pointsize=10)----
data(section, package='oce')
plot(section)

## ----eval=FALSE---------------------------------------------------------------
#  sub <- subset(ai, 1:2) # To subset for profiles
#  profiles <- getProfiles(sub, destdir='~/data/argo')
#  argos <- readProfiles(profiles, handleFlags = TRUE)
#  pressure <- argos[['argos']][[1]][['pressure']]
#  temperature <- argos[['argos']][[1]][['temperature']]
#  plot(temperature, pressure, ylim=rev(range(pressure, na.rm=TRUE)),
#       xlab='Temperature (C)', ylab='Depth (dbar)')

## ---- eval=FALSE--------------------------------------------------------------
#  ai <- getIndex(file='argo', destdir='~/data/argo')
#  # Subsetting by circle
#  aiCircle <- subset(ai, circle=list(longitude=-77.5, latitude=27.5, radius=50))
#  aiCircle # To determine it is type 'index'
#  # Subsetting by polygon
#  lonPoly <- c(-76.5, -76.0, -75.5)
#  latPoly <- c(25.5, 26.5, 25.5)
#  aiPoly <- subset(ai, polygon=list(longitude=lonPoly, latitude=latPoly))
#  aiPoly # To determine it is type 'index'
#  #Subset by time
#  from <- as.POSIXct("2013-01-01", tz="UTC")
#  to <- as.POSIXct("2013-12-31", tz="UTC")
#  aic <- subset(aiCircle, time=list(from=from, to=to))
#  aip <- subset(aiPoly, time=list(from=from, to=to))
#  # Plotting the subsets together
#  cp <- merge(aic, aip)
#  plot(cp)

## ---- eval=FALSE--------------------------------------------------------------
#  library(argoFloats)
#  library(oce)
#  data(section, package='oce')
#  lat0 <- median(section[['latitude']])
#  lon0 <- median(section[['longitude']])
#  # Subset by rectangle
#  ai <- getIndex(file='argo', destdir='~/data/argo')
#  latRect <- lat0 + c(-2,2)
#  lonRect <- lon0 + c(-30,30)
#  air <- subset(ai, rectangle=list(longitude=lonRect, latitude=latRect))
#  # Subset the rectangle by time
#  from <- as.POSIXct("2017-01-01", tz="UTC")
#  to <- as.POSIXct("2017-12-31", tz="UTC")
#  aiTime <- subset(air, time=list(from=from, to=to))
#  #Plot this subset
#  plot(aiTime)

## ---- eval=FALSE--------------------------------------------------------------
#  ai <- getIndex(file='merge', destdir = '~/data/argo')
#  sub <- subset(ai, 1:2) # To subset for profiles
#  profiles <- getProfiles(sub, destdir='~/data/argo')
#  argos <- readProfiles(profiles, handleFlags = TRUE)
#  plot(argos, which='TS')

