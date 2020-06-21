if (!interactive()) png("exampleTS.png", width=7, height=3.5, unit="in", res=120, pointsize=8)
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
if (!interactive()) dev.off()

