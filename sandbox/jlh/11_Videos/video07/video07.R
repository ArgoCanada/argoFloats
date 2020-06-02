library(argoFloats)
library(oce)
if (!exists("ai")) {
    ai <- getIndex()
    index0 <- subset(ai, ID='6900388')
}

lon <- index0[["longitude"]]
lat <- index0[["latitude"]]
profile <- index0[['profile']]
t <- index0[["date"]]

if (!interactive()) png("video07.png", unit="in", width=7, height=7, pointsize=11, res=150)

cm <- colormap(t, col=oceColorsJet)
drawPalette(colormap=cm, tformat="%Y-%m", pos=3)
plot(index0, bathymetry=TRUE)
points(lon, lat, pch=21, cex=1, bg=cm$zcol, col="black", lwd=1.2)
lines(lon, lat)
sub <- seq(0, length(lon), by=10)
text(lon[sub], lat[sub], profile[sub], cex=2/3, pos=1)
mtext(paste("Float 6900388"), adj=0)
