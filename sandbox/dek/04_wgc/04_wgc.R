rm(list=ls())
setup <- TRUE
library(oce)
library(ocedata)
library(argoFloats)
if (!interactive()) png("04_wgc.png")
par(mfrow=c(1, 2))
circle <- list(longitude=-55, latitude=63, radius=500)
data(coastlineWorldFine, package="ocedata")
plot(coastlineWorldFine, clongitude=circle$longitude, clatitude=circle$latitude, span=2000)
points(circle$longitude, circle$latitude, col=2, pch=20)

i <- getIndex()
s <- subset(i, circle=circle)
plot(s, which="map")
if (!interactive()) dev.off()

