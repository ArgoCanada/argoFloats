library(oce)
library(ocedata)
library(argoFloats)
source("../../../R/subset.R")
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


axy <- list(x = c(-83.58, -83.83, -83.3, -82.51, -81.65, -80.40, -79.35,
                  -78.42, -78.51),
            y = c(13.639, 11.97, 10.19, 9.44, 8.84, 9.01, 9.38, 9.405, 13.53))

bxy <- list(x = c(-86.70, -85.87, -84.52, -83.26, -81.73, -80.21, -78.49,
                  -78.53, -87.62, -87.65),
            y = c(11.62, 10.50, 9.72, 8.73, 8.24, 8.42, 8.66, 4.38, 4.39,
                  11.64))

plot(s, which="map")
polygon(axy, border="blue", lwd=3)
A <- subset(s, polygon=list(longitude=axy$x, latitude=axy$y))
points(A[["longitude"]], A[["latitude"]], col="blue", pch=20)

polygon(bxy, border="red", lwd=3)
P <- subset(s, polygon=list(longitude=bxy$x, latitude=bxy$y))
points(P[["longitude"]], P[["latitude"]], col="red", pch=20)

bad <- which(-81 < P[["longitude"]] & 9 < P[["latitude"]])
## bad
## [1] 1081 1082
## P[["index"]][bad,]
##                                         file                date latitude longitude ocean profiler_type institution         date_update
## 541899 aoml/4902065/profiles/R4902065_123.nc 2016-07-02 01:37:14    9.176   -80.867     A           846          AO 2018-10-05 17:29:14
## 542310 aoml/4902068/profiles/R4902068_077.nc 2015-12-23 06:49:27   10.405   -80.190     A           846          AO 2018-10-05 17:30:46

## Self-contained demonstration of error
x <- c(-80.867, -80.190)
y <- c(9.176, 10.405)
polx <- c(-86.70, -85.87, -84.52, -83.26, -81.73, -80.21, -78.49, -78.53, -87.62, -87.65, -86.70)
poly <- c(11.62, 10.50, 9.72, 8.73, 8.24, 8.42, 8.66, 4.38, 4.39, 11.64, 11.62)
polygon <- sf::st_polygon(list(outer=cbind(polx, poly)))
stopifnot(sf::st_is_valid(polygon))
points <- sf::st_multipoint(cbind(x, y))
stopifnot(sf::st_is_valid(points))
sf::st_intersection(points, polygon)
polygon(polx, poly)
points(x, y, col='red')

