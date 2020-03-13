# argoFloats

This R package provides tools for downloading and analyzing collections of
oceanographic Argo float datasets.

The code and graph below show an example of the construction of a
temperature-salinity diagram inferred from the 63 float profiles that were
within 160km of Sable Island, according to the Argo repository accessed in late
February of 2020.
```R
library(argoFloats)
library(oce)
# TS diagram using Argo profiles near Sable Island.
indexAll <- getIndex(destdir="~/data/argo")
# The next line yields 63 profiles as of February 2020
index <- subset(indexAll, circle=list(longitude=-59.915, latitude=44.934, radius=180))
profileIndex <- getProfiles(index)
argos <- readProfiles(profileIndex)
# Note: the following may be made moot by new functions.
argosList <- argos[["profile"]]
for (i in seq_len(length(argosList))) {
    a <- argosList[[i]]
    if (i == 1) {
        plotTS(a, Slim=c(31, 36), Tlim=c(1,22), eos="gsw")
    } else {
        points(a[["SA"]], a[["CT"]], col=i%%10)
    }
}
```

![Sample TS plot.](exampleTS.png)

