library(sf)

if (!interactive()) png("05_issue86c.png")
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(2, 0.7, 0))
load("test_data.rda")
asp <- 1/cos(pi/180*mean(range(y)))
plot(x, y, asp=asp, col="gray")
polygon(polx, poly, border="red")

Polygon <- sf::st_polygon(list(outer=cbind(polx, poly, rep(0, length(polx)))))

stopifnot(sf::st_is_valid(Polygon))
Points <- sf::st_multipoint(cbind(x, y, seq_along(x)))
stopifnot(sf::st_is_valid(Points))
Intersection <- sf::st_intersection(Points, Polygon)
points(Intersection[,1], Intersection[,2], col="red", lwd=2, pch=20)
keep <- rep(FALSE, length(x))
for (i in seq_along(x)) {
    keepx <- which(x[i] == Intersection[,1])
    keepy <- which(y[i] == Intersection[,2])
    keep[i] <- length(keepx) && length(keepy) && 1 == length(intersect(keepx, keepy))
}
points(x[keep], y[keep], col=3, pch=20, cex=0.5)

KEEP <- Intersection[,3]
points(x[KEEP], y[KEEP], col="black", pch=3)
if (!interactive()) dev.off()
