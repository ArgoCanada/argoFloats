library(argoFloats)
library(oce)
id0 <- '6900388'
if (!exists("index0"))
    index0 <- subset(getIndex(), id=id0)

lon <- index0[["longitude"]]
lat <- index0[["latitude"]]
cycle <- index0[['cycle']]
t <- index0[["date"]]

if (!interactive())
    png("video07.png", unit="in", width=7, height=7, pointsize=12, res=150)

par(mar=c(2,2,1,1))

cm <- colormap(t, col=oceColorsJet)
drawPalette(colormap=cm, tformat="%Y-%m", pos=3)
plot(index0, bathymetry=TRUE)
points(lon, lat, pch=21, cex=1, bg=cm$zcol, col="black", lwd=1.2)
lines(lon, lat)
sub <- seq(1, length(lon), by=10)
text(lon[sub], lat[sub], cycle[sub], cex=2/3, pos=1)
mtext(paste("Float", id0), adj=0)

if (!interactive())
    dev.off()

