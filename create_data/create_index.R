## An indexBgc of Argo profile data within 200km
## of Marsh Harbour, Abaco Island, Bahamas (26.54124N -77.0636W).
## https://github.com/dankelley/argoFloats/wiki/Focus-Island
##
## The download was done on 2020 March 14, and yielded 1788 profiles.

library(argoFloats)
indexAll <- getIndex(destdir="~/data/argo")
index <- subset(indexAll, circle=list(longitude=-77.06, latitude=26.54, radius=200))
save(index, file="index.rda")
tools::resaveRdaFiles('index.rda')

