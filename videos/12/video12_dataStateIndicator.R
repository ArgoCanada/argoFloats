## Note current code doesn't work. Need to fix subsetting by dataStateIndicator
## and handle more than one column in summary plot

# Jaimie 

library(argoFloats)
## Arabian Sea
index1 <- subset(getIndex(), circle=list(latitude=12.25, longitude=64.33, radius=40))
argos1 <- readProfiles(getProfiles(index1))
## Per-profile tabulation
table(unlist(argos1[["dataStateIndicator"]]))
## Get and test subset
argos2 <- subset(argos1, dataStateIndicator="2C")
argos2[["dataStateIndicator"]]

# Dan

par(mar=c(2,2,1,1))
plot(argos, which="map", bathymetry=FALSE)
lon2 <- unlist(argos2[["longitude"]])
lat2 <- unlist(argos2[["latitude"]])
points(lon2, lat2, col=4, cex=2)
