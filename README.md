# argoFloats

[![TravisCI Build Status](https://travis-ci.org/ArgoCanada/argoFloats.svg?branch=develop)](https://travis-ci.org/ArgoCanada/argoFloats)
[![codecov](https://codecov.io/gh/ArgoCanada/argoFloats/branch/develop/graph/badge.svg)](https://codecov.io/gh/ArgoCanada/argoFloats)

The argoFloats package for the R language provides tools for downloading and
analyzing collections of oceanographic Argo float datasets.  Its developers are
* Dan Kelley, Dalhousie University
* Jaimie Harbin, Bedford Institute of Oceanography and Dalhousie University
* Clark Richards, Bedford Institute of Oceanography

Since argoFloats is in an active phase of development, it is not yet
available on CRAN.  Still, it is easily installed in R with
```R
library(devtools)
install_github('ArgoCanada/argoFloats', ref='develop')
```
where, of course, the `devtools` package must be installed first, if it is not
already present on the user's system.  It is also necessary to have reasonably
up-to-date versions of the `oce` and `ocedata` packages installed, which is
accomplished with
```R
install_github('dankelley/oce', ref='develop')
install_github('dankelley/ocedata', ref='develop')
```

Once things are set up as above, it will be possible to use all the features of
`argoFloats`, many of which are illustrated in the documentation for its
functions, and in the vignette that is built into the package, both of which
are displayed on the [user-oriented
website](https://argocanada.github.io/argoFloats/index.html). Note that the
vignette also lists youtube videos about the package.

For example, the following shows how to create a map and a temperature-salinity
diagram for several Argo float profiles made near Abaco Island in the Bahamas.
First, the `getIndex` function is used to download a worldwide index of float
profiles.  (Use `?getIndex` to learn more about this function, and note in
particular the `destdir` argument, which determines where the index and other
argo files will be stored locally.) Next, the `subset` function is used to
narrow the region of interest; yielding (as of mid-June 2020) 39 profiles.  The
`getProfiles` function is then used to download the relevant netcdf files that
contain the profile data. Then, `readProfiles` is used to read those files.
Many profiles contain quality-control flags to indicate bad or questionable
data, and the `applyQC` is used to set such data to `NA`.  Finally, a map plot
is drawn, with a label indicating the number of profiles shown; the latter
illustrates the use of `[[`, a generic R function that is used to access data
and metadata elements in argoFloats objects.  Finally, a temperature-salinity
plot is shown.  As an exercise, the reader might find it informative to repeat
the procedure, without the QC step.
```R
library(argoFloats)
library(oce)
## Get worldwide float-profile index, saving to ~/data/argo by default.
indexAll <- getIndex()
## Narrow to a 30km-radius circle centred on Abaco Island, The Bahamas.
index <- subset(indexAll,
                circle=list(longitude=-77.06,latitude=26.54,radius=30))
## Get netcdf files for these profiles, saving to ~/data/argo by default.
profiles  <- getProfiles(index)
## Read the netcdf files.
argos <- readProfiles(profiles)
## Examine QC flags, and set questionable data to NA.
argosClean <- applyQC(argos)
## Set up a two-panel plot.
par(mfrow=c(1, 2))
## Tighten margins (optional).
par(mar=c(3.5, 3.5, 2.0, 2.0))
## Plot a map with bathymetry, indicating number of profiles.
plot(index, which="map")
mtext(paste(argosClean[["length"]], "profiles"))
## Plot a TS diagram
plot(argosClean, which="TS")
```
![Sample TS plot.](exampleTS.png)

