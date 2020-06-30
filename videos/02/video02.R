library(argoFloats)
# Get world index
data(index)
# Get index withing 50km of Bermuda
ais <- subset(index, 1:10)
# Get profile data files for this near-Bermuda index
pr <- getProfiles(ais)
# Read those data files
a <- readProfiles(pr, handleFlags=TRUE)
# Plot a map to the left, and a TS diagram to the right
if (!interactive()) png("142a.png")
par(mfrow=c(1, 2))
plot(a, which="map")
plot(a, which='TS')
if (!interactive()) dev.off()
