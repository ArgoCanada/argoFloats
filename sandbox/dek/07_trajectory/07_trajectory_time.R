library(argoFloats)
library(oce)
library(cmocean)
data(index)
id <- index[["id"]]
cycle <- index[["cycle"]]
tableSorted <- sort(table(id))
## isolate the float that has the most cycles
id0 <- names(tail(tableSorted, 1))
index0 <- subset(index, id=id0)
o <- order(as.numeric(index0[["cycle"]]))
lon <- index0[["longitude"]][o]
lat <- index0[["latitude"]][o]
cycle <- index0[["cycle"]][o]
t <- index0[["date"]][o]

if (!interactive()) png("07_trajectory_time.png", unit="in", width=7, height=7, pointsize=11, res=150)
par(mar=c(3,3,1,1))
#cm <- colormap(t, col=cmocean("jet"))
cm <- colormap(t, col=oceColorsJet)
drawPalette(colormap=cm, tformat="%Y-%m", pos=3)
plot(index0, bathymetry=TRUE)
points(lon, lat, pch=21, cex=1.8, col="darkgrey", bg=cm$zcol, lwd=1.8)
points(-77.06,26.54, pch="+")
lines(lon, lat)
sub <- seq(0, length(lon), by=10)
text(lon[sub], lat[sub], cycle[sub], cex=2/3, pos=1)
mtext(paste("Float", id0), adj=0)
mtext(paste(format(range(t), "%b %Y"), collapse= " to "), adj=1)
if (!interactive()) dev.off()

