# Accelerating current speed?
library(argoFloats)
library(oce)
lonRect <- c(-60.520, -21.385)
latRect <- c(48.743, 64.335)
if(!exists("index1"))
    index1 <- subset(getIndex(), rectangle=list(longitude=lonRect, latitude=latRect))
# First 10 years of this geography
from <- as.POSIXct("1999-01-01", tz="UTC")
to <- as.POSIXct("2009-01-01", tz="UTC")
index2 <- subset(index1, time=list(from=from, to=to))
id <- index2[["id"]]
cycle <- index2[["cycle"]]
t <- sort(table(id))
## isolate the float that has the most cycles
id0 <- names(tail(t, 1))

index0 <- subset(index2, id=id0)
o <- order(as.numeric(index0[["cycle"]]))
lon <- index0[["longitude"]][o]
lat <- index0[["latitude"]][o]
cycle <- index0[["cycle"]][o]
t <- index0[["date"]][o]

dist <- 1000 * geodDist(lon, lat, alongPath=TRUE)
speed <- diff(dist) / diff(as.numeric(t))
speed <- c(speed[1], speed)

#if (!interactive()) png("trajectory_speed.png", unit="in", width=7, height=7, pointsize=11, res=150)
par(mar=c(3,3,1,1))
cm <- colormap(speed, col=oceColorsViridis)
drawPalette(colormap=cm, zlab="Speed [m/s]", pos=3)
plot(index0)
points(lon, lat, pch=20, cex=2, col=cm$zcol)
lines(lon, lat)
sub <- seq(0, length(lon), by=10)
text(lon[sub], lat[sub], cycle[sub], cex=2/3, pos=1)
mtext(paste("Float", id0), adj=0)
mtext(paste(format(range(t), "%b %Y"), collapse= " to "), adj=1)
#if (!interactive()) dev.off()

## Now do the same for the last 10 years compared to first 10 years
library(argoFloats)
library(oce)
lonRect <- c(-60.520, -21.385)
latRect <- c(48.743, 64.335)
if(!exists("index1"))
    index1 <- subset(getIndex(), rectangle=list(longitude=lonRect, latitude=latRect))
# First 10 years of this geography
from <- as.POSIXct("2009-01-01", tz="UTC")
to <- as.POSIXct("2019-01-01", tz="UTC")
index2 <- subset(index1, time=list(from=from, to=to))
id <- index2[["id"]]
cycle <- index2[["cycle"]]
t <- sort(table(id))
## isolate the float that has the most cycles
id0 <- names(tail(t, 1))

index0 <- subset(index2, id=id0)
o <- order(as.numeric(index0[["cycle"]]))
lon <- index0[["longitude"]][o]
lat <- index0[["latitude"]][o]
cycle <- index0[["cycle"]][o]
t <- index0[["date"]][o]

dist <- 1000 * geodDist(lon, lat, alongPath=TRUE)
speed <- diff(dist) / diff(as.numeric(t))
speed <- c(speed[1], speed)

#if (!interactive()) png("trajectory_speed.png", unit="in", width=7, height=7, pointsize=11, res=150)
par(mar=c(3,3,1,1))
cm <- colormap(speed, col=oceColorsViridis)
drawPalette(colormap=cm, zlab="Speed [m/s]", pos=3)
plot(index0)
points(lon, lat, pch=20, cex=2, col=cm$zcol)
lines(lon, lat)
sub <- seq(0, length(lon), by=10)
text(lon[sub], lat[sub], cycle[sub], cex=2/3, pos=1)
mtext(paste("Float", id0), adj=0)
mtext(paste(format(range(t), "%b %Y"), collapse= " to "), adj=1)