## An index of "synthetic" profiles (a blend of core argo and biogeochemical
## argo ... the new form of "merged" files, as of late 2019 and early 2020) from within 300km
## of Marsh Harbour, Abaco Island, Bahamas (26.54124N -77.0636W).
## For the selection of this location, see
## https://github.com/dankelley/argoFloats/wiki/Focus-Island
##
## Note that this code is not run, because we want the dataset to be *static*.
## In fact, we check that it is unchanged, in the test suite.
##
## The download was done on 2020 September 10, and yielded 39 profiles. Note that this
## failed with the usgodae server on that date, because it was not yet serving
## synthetic files, but this is not a problem because getIndex() found the
## ifremer server after it found the usgodae server to be deficient.

if (!FALSE) {
    library(argoFloats)
    indexAll <- getIndex("synthetic", age=0)
    indexSynthetic <- subset(indexAll, circle=list(longitude=-77.06, latitude=26.54, radius=300))
    save(indexSynthetic, file="indexSynthetic.rda")
    tools::resaveRdaFiles('indexSynthetic.rda')
}
