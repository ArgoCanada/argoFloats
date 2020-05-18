library(argoFloats)
library(oce)
debug <- FALSE # use to find first and last points
data(index)
ID <- index[["ID"]]
profile <- index[["profile"]]
t <- sort(table(ID))
## isolate the float that has the most profiles
ID0 <- names(tail(t, 1))
index0 <- subset(index, ID=ID0)
o <- order(as.numeric(index0[["profile"]]))
lon <- index0[["longitude"]][o]
lat <- index0[["latitude"]][o]
profile <- index0[["profile"]][o]
t <- index0[["date"]][o]

dist <- 1000 * geodDist(lon, lat, alongPath=TRUE)
speed <- diff(dist) / diff(as.numeric(t))
speed <- c(speed[1], speed)
## plot(t, speed, type='o')

par(mar=c(3,3,1,1))
cm <- colormap(speed, col=oceColorsViridis)
drawPalette(colormap=cm, zlab="Speed [m/s]")
plot(index0)
points(lon, lat, pch=20, cex=2, col=cm$zcol)
lines(lon, lat)
sub <- seq(0, length(lon), by=10)
text(lon[sub], lat[sub], profile[sub], cex=2/3, pos=1)
mtext(paste("Float", ID0), adj=0)
mtext(paste(format(range(t), "%b %Y"), collapse= " to "), adj=1)

if (debug) {
    n <- length(lon)
    text(lon[1], lat[1], "Start")
    text(lon[n], lat[n], "End")
}

