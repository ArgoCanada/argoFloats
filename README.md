# argoFloats

This R package provides tools for downloading and analyzing collections of
oceanographic Argo float datasets.

The following shows how to create a map and a temperature-salinity diagram for
several Argo float profiles made near Abaco Island in the Bahamas.  The
`getIndex()` call specifies a directory to hold the index of float profiles,
and this directory carries through to the `getProfiles()` call, which downloads
the netcdf files that contain the profile data, and later to the
`readProfiles()` call, which reads those files.
```R
library(argoFloats)
library(oce)
# Profiles near Abaco Island, Bahamas.
indexAll <- getIndex(destdir="~/data/argo")
index <- subset(indexAll,
                circle=list(longitude=-77.06,latitude=26.54,radius=30))
profiles  <- getProfiles(index)
argos <- readProfiles(profiles, handleFlags=TRUE)
par(mfrow=c(1, 2))
plot(index, which="map")
plot(argos, which="TS")
```
![Sample TS plot.](exampleTS.png)

