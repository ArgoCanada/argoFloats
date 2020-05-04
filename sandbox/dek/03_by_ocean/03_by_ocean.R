library(oce)
library(ocedata)
library(argoFloats)
source("~/git/argoFloats/R/subset.R")
setup <- FALSE

i <- getIndex()
s <- subset(i, circle=list(longitude=-83, latitude=9, radius=500))
if (!interactive()) png("03_by_ocean_1.png")
plot(s, which="map")
atlantic <- subset(s, ocean="A")
pacific <- subset(s, ocean="P")
points(atlantic[["longitude"]], atlantic[["latitude"]], pch=20, col=2, cex=0.7*par("cex"))
points(pacific[["longitude"]], pacific[["latitude"]], pch=20, col=3, cex=0.7*par("cex"))
if (!interactive()) dev.off()

if (!interactive()) png("03_by_ocean_2.png")


if (setup) {
    axy <- locator(20, type="o")
    sink('a');dput(axy);sink()
} else {
    axy <- list(x = c(-83.58, -83.83, -83.3, -82.51, -81.65, -80.40, -79.35,
                      -78.42, -78.51),
                y = c(13.639, 11.97, 10.19, 9.44, 8.84, 9.01, 9.38, 9.405, 13.53))
}

if (setup) {
    bxy <- locator(20, type="o")
    sink('b');dput(bxy);sink()
} else {
    bxy <- list(x = c(-86.70, -85.87, -84.52, -83.26, -81.73, -80.21, -78.49,
                      -78.53, -87.62, -87.65),
                y = c(11.62, 10.50, 9.72, 8.73, 8.24, 8.42, 8.66, 4.38, 4.39,
                      11.64))
}

plot(s, which="map")
polygon(axy, border="blue", lwd=3)
A <- subset(s, polygon=list(longitude=axy$x, latitude=axy$y))
points(A[["longitude"]], A[["latitude"]], col="blue", pch=20)

polygon(bxy, border="red", lwd=3)
P <- subset(s, polygon=list(longitude=bxy$x, latitude=bxy$y))
points(P[["longitude"]], P[["latitude"]], col="red", pch=20)

## 1081 1082
bad <- which(-82 < P[["longitude"]] & 9 < P[["latitude"]])
if (length(bad)) {
    message("had ", length(bad), " bad points")
    points(P[["longitude"]][bad], P[["latitude"]][bad], col="red", pch=20, cex=2)
    text(P[["longitude"]][bad], P[["latitude"]][bad], bad, col="red", font=2, pos=1, cex=2)
}

if (!interactive()) dev.off()
