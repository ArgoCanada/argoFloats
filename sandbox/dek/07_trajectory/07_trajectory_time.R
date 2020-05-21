library(argoFloats)
library(oce)
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

if (!interactive()) png("07_trajectory_time.png", unit="in", width=7, height=7, pointsize=11, res=150)
par(mar=c(3,3,1,1))
cm <- colormap(t, col=oceColorsViridis)
drawPalette(colormap=cm, tformat="%Y-%m", pos=3)
plot(index0, bathymetry=TRUE)
points(lon, lat, pch=21, cex=1.4, col="black", bg=cm$zcol)
lines(lon, lat)
sub <- seq(0, length(lon), by=10)
text(lon[sub], lat[sub], profile[sub], cex=2/3, pos=1)
mtext(paste("Float", ID0), adj=0)
mtext(paste(format(range(t), "%b %Y"), collapse= " to "), adj=1)
if (!interactive()) dev.off()

