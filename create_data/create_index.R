## An index of data near Sable Island, at the shelf break south of Halifax,
## Nova Scotia.  This produced 10 profiles, as of 2020 March 14.

library(argoFloats)
library(oce)
indexAll <- getIndex(destdir="~/data/argo")
## NB radius=160 gives 10 profiles
## NB radius=300 gives 1783 profiles
index <- subset(indexAll, circle=list(longitude=-59.91, latitude=43.93, radius=300))
save(index, file="index.rda")
tools::resaveRdaFiles('index.rda')

