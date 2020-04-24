library(argoFloats)
#library(oce)
# Get world index
ai <- getIndex()
# Get index withing 50km of Bermuda
ais <- subset(ai, circle=list(longitude=-64.7505, latitude=32.3078, radius=50))
# Get profile data files for this near-Bermuda index
pr <- getProfiles(ais)
# Read those data files
a <- readProfiles(pr)
# Plot a map to the left, and a TS diagram to the right
par(mfrow=c(1, 2))
plot(a, which="map")
plot(a, which="TS")
