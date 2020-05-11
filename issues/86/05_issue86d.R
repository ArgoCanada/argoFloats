library(argoFloats)

i <- getIndex()
circle <- subset(i, circle=list(longitude=-83, latitude=9, radius=500))
p <- list(longitude = c(-86.7, -85.87, -84.52, -83.26, -81.73, -80.21, -78.49, -78.53, -87.62,
                        -87.65, -86.7),
          latitude = c(11.62, 10.5, 9.72, 8.73, 8.24, 8.42, 8.66, 4.38, 4.39, 11.64, 11.62))

if (!interactive()) png("05_issue86d.png")
par(mar=c(2.5,2.5,0.5,0.5), mgp=c(2, 0.7, 0))
plot(circle)
ps <- subset(circle, polygon=p)
points(ps[["longitude"]], ps[["latitude"]], col=2, cex=1)
polygon(p[["longitude"]], p[["latitude"]], border=2, lwd=2)
if (!interactive()) dev.off()
