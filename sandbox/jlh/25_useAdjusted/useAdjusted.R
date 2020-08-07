# Example
library(argoFloats)
raw <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"))
adj <- useAdjusted(raw)
rawO <- unlist(raw[['oxygen']])
rawP <- unlist(raw[['pressure']])
plot(rawO, rawP, ylim=rev(range(rawP, na.rm=TRUE)), pch=16, col='blue')
points(unlist(adj[['oxygen']]), unlist(adj[['pressure']]), ylim=rev(range(unlist(adj[['pressure']]), na.rm=TRUE)), pch=16, col='red')

stop()


#Exercise
library(argoFloats)
bai <- getIndex('synthetic')
s <- subset(bai, id='5903586')
ss <- subset(s, 50)
profiles <- getProfiles(ss)
raw <- readProfiles(profiles)
adj <- useAdjusted(raw)
rawO <- unlist(raw[['oxygen']])
rawP <- unlist(raw[['pressure']])
adjO <- unlist(adj[['oxygen']])
ajdP <- unlist(adj[['pressure']])
plot(rawO, rawP, ylim=rev(range(rawP, na.rm=TRUE)), pch=16, col='blue')
points(unlist(adjO), unlist(adjP), ylim=rev(range(rawP, na.rm=TRUE)), pch=16, col='red')
