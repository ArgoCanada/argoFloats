## https://github.com/ArgoCanada/argoFloats/issues/227
library(argoFloats)
library(ocedata)
library(oce)
par(mar=c(3,3,1,3))
data("coastlineWorldMedium")
ai <- getIndex(age=10) # no need to download very latest data
from <- as.POSIXct("2019-07-01 00:00:00", tz="UTC")
to <- as.POSIXct("2019-07-02 00:00:00", tz="UTC")
subset <- subset(ai, time=list(from=from, to=to))
plot(subset)
plot(subset, bathymetry=FALSE)
plot(subset, xlim=c(0, 50), ylim=c(0,90))
plot(subset, xlim=c(0, 50), ylim=c(0,90),bathymetry=FALSE)

