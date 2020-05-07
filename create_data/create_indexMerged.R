## An indexBgc of biogeochemical Argo profile data within 300km
## of Marsh Harbour, Abaco Island, Bahamas (26.54124N -77.0636W).
## For the selection of this location, see
## https://github.com/dankelley/argoFloats/wiki/Focus-Island
##
## Note that this code is not run, because we want the dataset to be *static*.
## In fact, we check that it is unchanged, in the test suite.
##
## The download was done on 2020 March 23, and yielded 39 profiles.

if (FALSE) {
    library(argoFloats)
    indexAll <- getIndex(file="merged", destdir="~/data/argo")
    indexMerged <- subset(indexAll, circle=list(longitude=-77.06, latitude=26.54, radius=300))
    save(indexMerged, file="indexMerged.rda")
    tools::resaveRdaFiles('indexMerged.rda')
}
