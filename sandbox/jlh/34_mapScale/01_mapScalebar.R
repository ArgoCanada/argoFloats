# This is for projection type

library(oce)
data(argo)
plot(argo)
usr <- par("usr")
# Determining scale from the center
x0 <- 0.5 * (usr[1] + usr[2])
y0 <- 0.5 * (usr[3] + usr[4])
dusr <- 0.01 * (usr[2] - usr[1]) # 1 percent of device width
x1 <- x0 + dusr # Taking centre plus 1%
y1 <- y0 # Staying the same, only worried about horizontal

# Converts from x-y coordinates to longitude and latitude
lonlat0 <- map2lonlat(x0, y0) # Where you're starting
lonlat1 <- map2lonlat(x1, y1) # Where you're going to (in lat/lon)
# Finding the distance to where you went
dkm <- geodDist(lonlat0$longitude, lonlat0$latitude,
                lonlat1$longitude, lonlat1$latitude)

kmPerUsr <- dkm / dusr

ccd <- kmPerUsr * sqrt( (usr[2]-usr[1])^2 + (usr[4]-usr[3])^2 )
length <- diff(pretty(c(0, ccd), n=12)[1:2])
sprintf("%.3f km", length)

# This is when projection is false

usr <- par('usr')

## determine scale from centre of region
x0 <- 0.5 * (usr[1] + usr[2])
y0 <- 0.5 * (usr[3] + usr[4])
dusr <- 0.01 * (usr[2] - usr[1]) # 1 percent of device width
x1 <- x0 + dusr
y1 <- y0
dkm <- geodDist(x0, y0,
                x1, y1)
kmPerUsr <- dkm / dusr


