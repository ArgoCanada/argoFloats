library(argoFloats)
library(oce)
#library(cmocean)
data(index)
ID <- index[["ID"]]
profile <- index[["profile"]]
tableSorted <- sort(table(ID))
## isolate the float that has the most profiles
ID0 <- names(tail(tableSorted, 1))
index0 <- subset(index, ID=ID0)
o <- order(as.numeric(index0[["profile"]]))
lon <- index0[["longitude"]][o]
lat <- index0[["latitude"]][o]
profile <- index0[["profile"]][o]
t <- index0[["date"]][o]

if (!interactive()) png("10_trajectory_time.png", unit="in", width=7, height=3.2, pointsize=11, res=150)
par(mfrow=c(1,2))
bathy <- marmap::getNOAA.bathy(-82, -71, 23, 30, 2, keep=TRUE)
par(mar=c(3,3,1,1))
plot(index, bathymetry=list(source=bathy, debug=1))
usr <- par('usr')
#cm <- colormap(t, col=cmocean("jet"))
cm <- colormap(t, col=oceColorsJet)
par(mar=c(3,3,1,1))
drawPalette(colormap=cm, tformat="%Y-%m", pos=4)
plot(index0, xlim=usr[1:2], ylim=usr[3:4], bathymetry=list(source= bathy, palette=FALSE), debug=1)
points(lon, lat, pch=21, cex=1, col="darkgrey", bg=cm$zcol, lwd=1.5)
points(-77.06,26.54, pch="*", cex=1.8, col='black')
lines(lon, lat)
sub <- seq(0, length(lon), by=10)
text(lon[sub], lat[sub], profile[sub], cex=2/3, pos=1)
mtext(paste("Float", ID0), adj=0)
#mtext(paste(format(range(t), "%b %Y"), collapse= " to "), adj=1)
if (!interactive()) dev.off()

