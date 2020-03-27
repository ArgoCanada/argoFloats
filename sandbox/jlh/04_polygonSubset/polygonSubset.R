# Without coastLine

latitudePolygon <- c(24, 27, 24) # Creating a polygon that closes
longitudePolygon <- c(-79, -78, -74 )
plot(longitudePolygon, latitudePolygon, xlim=c(-80,-74),
     ylim=c(20,30), type="l", lwd=5, col="blue")
abacaSub <- subset(index, circle=list(longitude=-77.15, latitude=26.35, radius=300))
latitudePoint <- abacaSub[['latitude']]
longitudePoint <- abacaSub[['longitude']]
indexP <- subset(index, polygon=list(latitude=latitudePolygon, longitude=longitudePolygon))
points(longitudePoint, latitudePoint, col=inside+1, pch=20)


# With coastLine

library(oce)
data(package='ocedata')
data('coastlineWorldFine', package='ocedata')
latitudePolygon <- c(24, 27, 24) # Creating a polygon that closes
longitudePolygon <- c(-79, -78, -74 )
mapPlot(coastlineWorldFine, col='lightgray', longitudelim=c(-83,-71), latitudelim=c(20,30), projection="+proj=merc", grid=TRUE)
mapPoints(longitudePolygon, latitudePolygon, type="l", lwd=5, col="blue")
abacaSub <- subset(index, circle=list(longitude=-77.15, latitude=26.35, radius=300))
latitudePoint <- abacaSub[['latitude']]
longitudePoint <- abacaSub[['longitude']]
indexP <- subset(index, polygon=list(latitude=latitudePolygon, longitude=longitudePolygon))
mapPoints(longitudePoint, latitudePoint, col=inside+1, pch=20)
