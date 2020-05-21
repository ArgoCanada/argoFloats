library(argoFloats)
library(oce)
## Cache a slow downloading operation, for speed of code development
if (!exists("ai")) {
    ai <- getIndex()
    s <- subset(ai, circle=list(longitude=-83, latitude=9, radius=150))
}

pax <- c(-83.31811, -81.46290, -81.46290, -82.33111, -83.10792, -83.40950)
pay <- c(10.365979, 10.365979,  8.876569,  9.048076,  9.698001, 10.384033)
# Now select your points on the map in the Atlantic
a <- subset(s, polygon=list(longitude=pax, latitude=pay))
ppx <- c(-84.50618, -83.41864, -81.82846, -81.60913, -83.19931, -84.60671, -84.61585)
ppy <- c(9.643841, 8.939756, 7.919735, 7.621853, 7.630880, 7.657960, 9.445253)
p <- subset(s, polygon=list(longitude=ppx, latitude=ppy))
profilesA <- getProfiles(a)
argosA <- readProfiles(profilesA)
profilesP <- getProfiles(p)
argosP <- readProfiles(profilesP)
CT <- unlist(argosP[['CT']]) # NOTE: we use "gsw" eqn of state in TS plot
SA <- unlist(argosP[['SA']])
## Demonstrate EOS things
##> S <- unlist(argosP[['salinity']])
##> T <- unlist(argosP[['temperature']])
##> plot(T, CT, type='l')
##> lines(T, theta, col=2)
##> abline(0, 1, col=3)
##> plot(S, SA, type='l')
##> abline(0, 1, col=2)

if (!interactive()) pdf("08_polygonPlot.pdf", width=7, height=3.5, pointsize=11)

par(mfrow=c(1,2), mar=c(3,3,1,1), mgp=c(2,0.7,0))
## Map
plot(s, which='map')
legend("topleft", c("Atlantic","Pacific"), col=c("red","blue"), pch=c(20,20), bg="white")
points(a[['longitude']], a[['latitude']], pch=20, col='red')
points(p[['longitude']], p[['latitude']],pch=20, col='blue')

## TS Diagram, contrasting Atlantic and Pacific
plot(argosA, which='TS', col='red', xlim=c(30,38), ylim=c(0,30))
points(SA, CT, pch=20, col='blue')

if (!interactive()) dev.off()

