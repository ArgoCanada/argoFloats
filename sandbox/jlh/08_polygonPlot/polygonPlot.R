par(mfrow=c(1,2))
library(argoFloats)
library(oce)
ai <- getIndex()
s <- subset(ai, circle=list(longitude=-83, latitude=9, radius=150))
plot(s, which='map', legend("topright", c("Atlantic","Pacific"), col=c("red","blue"), pch=c(20,20))) # To get a visual
legend("bottomleft", c("Atlantic","Pacific"), col=c("red","blue"), pch=c(20,20))
pax <- c(-83.31811, -81.46290, -81.46290, -82.33111, -83.10792, -83.40950)
pay <- c(10.365979, 10.365979,  8.876569,  9.048076,  9.698001, 10.384033)
# Now select your points on the map in the Atlantic
a <- subset(s, polygon=list(longitude=pax, latitude=pay))
points(a[['longitude']], a[['latitude']], pch=20, col='red')
ppx <- c(-84.50618, -83.41864, -81.82846, -81.60913, -83.19931, -84.60671, -84.61585)
ppy <- c(9.643841, 8.939756, 7.919735, 7.621853, 7.630880, 7.657960, 9.445253)
# Now select your points on the map in the Pacific
p <- subset(s, polygon=list(longitude=ppx, latitude=ppy))
points(p[['longitude']], p[['latitude']],pch=20, col='blue')

## Making TS Diagram for both Atlantic and Pacific
profilesA <- getProfiles(a)
argosA <- readProfiles(profilesA)
plot(argosA, which='TS', col='red', xlim=c(30,38), ylim=c(0,30))
par(new=TRUE)
profilesP <- getProfiles(p)
argosP <- readProfiles(profilesP)
T <- unlist(argosP[['temperature']])
S <- unlist(argosP[['salinity']])
points(S,T, pch=20, col='blue')