if (!interactive()) png("bathymetry.png", unit="in", width=7, height=4, pointsize=11, res=100)
library(argoFloats)
library(oce)
ID0 <- '6900388'
if (!exists("index0"))
    index0 <- subset(getIndex(), ID=ID0)

lon <- index0[["longitude"]]
lat <- index0[["latitude"]]
profile <- index0[['profile']]
t <- index0[["date"]]

par(mar=c(2,2,1,1))

cm <- colormap(t, col=oceColorsJet)
drawPalette(colormap=cm, tformat="%Y-%m", pos=3)
plot(index0, bathymetry=TRUE)
points(lon, lat, pch=21, cex=1, bg=cm$zcol, col="black", lwd=1.2)
lines(lon, lat)
sub <- seq(1, length(lon), by=10)
text(lon[sub], lat[sub], profile[sub], cex=2/3, pos=1)
mtext(paste("Float", ID0), adj=0)
if (!interactive()) dev.off()