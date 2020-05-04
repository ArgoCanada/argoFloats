library(oce)
library(ocedata)
library(argoFloats)
if (FALSE) { # finding good center
    data(coastlineWorldFine, package="ocedata")
    plot(coastlineWorldFine, clongitude=-80, clatitude=8, span=4000)
    ## centre <- locator(1)
    centre <- list(longitude=-83, latitude=9)
    points(centre$longitude, centre$latitude, col=2, pch=20)
    lines(rep(centre$longitude,2), c(-5,5)+centre$latitude, col=2) # show diameter
}

i <- getIndex()
s <- subset(i, circle=list(longitude=-83, latitude=9, radius=500))
if (!interactive()) png("by_ocean.png")
plot(s, which="map")
atlantic <- subset(s, ocean="A")
pacific <- subset(s, ocean="P")
points(atlantic[["longitude"]], atlantic[["latitude"]], pch=20, col=2, cex=0.7*par("cex"))
points(pacific[["longitude"]], pacific[["latitude"]], pch=20, col=3, cex=0.7*par("cex"))
if (!interactive()) dev.off()

