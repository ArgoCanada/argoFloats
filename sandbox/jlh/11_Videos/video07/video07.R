library(argoFloats)
library(oce)
if (!exists("ai")) {
    ai <- getIndex() 
    index0 <- subset(ai, ID='6900388')}
if (!interactive()) png("video07.png", unit="in", width=3.5, height=3.2, pointsize=11, res=150)
lon <- index0[["longitude"]]
lat <- index0[["latitude"]]
t <- index0[["date"]]
par(mar=c(3,3,1,1))
cm <- colormap(t, col=oceColorsJet)
drawPalette(colormap=cm, tformat="%Y-%m", pos=3)
plot(index0, bathymetry=TRUE)
points(lon, lat, pch=21, cex=1, bg=cm$zcol, col="black", lwd=1.2)
lines(lon, lat)
sub <- seq(0, length(lon), by=10)
text(lon[sub], lat[sub], profile[sub], cex=2/3, pos=1)
mtext(paste("Float", ID0), adj=0)
