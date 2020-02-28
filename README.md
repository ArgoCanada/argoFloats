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
index <- getIndex(destdir="~/data/argo")
# The next line yields 63 profiles as of February 2020
indexSI <- subset(index, circle=list(longitude=-59.915, latitude=44.934, radius=180))
profilesSI <- getProfiles(indexSI)
argosSI <- readProfiles(profilesSI)
for (i in seq_len(length(argosSI))) {
    argo <- argosSI[[i]]
    if (i == 1) {
        plotTS(argo, Slim=c(31, 36), Tlim=c(1,24))
    } else {
        SA <- argo[["SA"]]
        CT <- argo[["CT"]]
        points(SA, CT, col=i%%10)
    }
}
```

![exampleTS.png](exampleTS.png)

