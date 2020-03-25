## An indexBgc of biogeochemical Argo profile data within 300km
## of Marsh Harbour, Abaco Island, Bahamas (26.54124N -77.0636W).
## For the selection of this location, see
## https://github.com/dankelley/argoFloats/wiki/Focus-Island
##
## The download was done on 2020 March 23, and yielded 39 profiles.

library(argoFloats)
indexAll <- getIndex(file="bgc", destdir="~/data/argo")
indexBgc <- subset(indexAll, circle=list(longitude=-77.06, latitude=26.54, radius=300))
str(indexBgc)
save(indexBgc, file="indexBgc.rda")
tools::resaveRdaFiles('indexBgc.rda')

