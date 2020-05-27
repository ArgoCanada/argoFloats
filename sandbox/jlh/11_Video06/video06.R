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
if (!interactive()) png("video06.png", unit="in", width=3.5, height=3.2, pointsize=11, res=150)
cm <- colormap(t, col=oceColorsJet)
par(mar=c(3,3,1,1))
drawPalette(colormap=cm, tformat="%Y-%m", pos=4)
plot(index0, bathymetry=FALSE)
points(lon, lat, pch=21, cex=1, bg=cm$zcol, col="darkgrey", lwd=0.5)
points(-77.06,26.54, pch="*", cex=1.8, col='black')
lines(lon, lat)
sub <- seq(0, length(lon), by=10)
text(lon[sub], lat[sub], profile[sub], cex=2/3, pos=1)
mtext(paste("Float", ID0), adj=0)
        