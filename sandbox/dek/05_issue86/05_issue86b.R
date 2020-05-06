library(sf)

if (!interactive()) png("05_issue86b.png")

load("test_data.rda")
asp <- 1/cos(pi/180*mean(range(y)))
plot(x, y, asp=asp, col="gray")
polygon(polx, poly, border="red")

Polygon <- sf::st_polygon(list(outer=cbind(polx, poly)))

stopifnot(sf::st_is_valid(Polygon))
##Points <- sf::st_multipoint(cbind(x, y))
Points <- sf::st_multipoint(cbind(x, y))
stopifnot(sf::st_is_valid(Points))
Intersection <- sf::st_intersection(Points, Polygon)
points(Intersection[,1], Intersection[,2], col="pink", lwd=2, pch=20)
keep <- rep(FALSE, length(x))
for (i in seq_along(x)) {
    keepx <- which(x[i] == Intersection[,1])
    keepy <- which(y[i] == Intersection[,2])
    keep[i] <- length(keepx) && length(keepy) && 1 == length(intersect(keepx, keepy))
}
points(x[keep], y[keep], col=3, pch=20)

if (!interactive()) dev.off()

