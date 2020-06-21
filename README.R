if (!interactive()) png("exampleTS.png", width=7, height=3.5, unit="in", res=120, pointsize=8)
library(argoFloats)
library(oce)
## 1. Get worldwide float-profile index, saving to ~/data/argo by default.
indexAll <- getIndex()
## 2. Narrow to a 30km-radius circle centred on Abaco Island, The Bahamas.
index <- subset(indexAll,
                circle=list(longitude=-77.06,latitude=26.54,radius=30))
## 3. Get netcdf files for these profiles, saving to ~/data/argo by default.
profiles  <- getProfiles(index)
## 4. Read the netcdf files.
argos <- readProfiles(profiles)
## 5. Examine QC flags, and set questionable data to NA.
argosClean <- applyQC(argos)
par(mfrow=c(1, 2))                     # want two-panel plot
par(mar=c(3.5, 3.5, 2.0, 2.0))         # tighten margins
## 6. Plot a map with bathymetry, indicating number of profiles.
plot(index, which="map")
points(-77.06, 26.54, pch="*", cex=3)  # show centre of focus
mtext(paste(argosClean[["length"]], "profiles"))
## 7. Plot a TS diagram
plot(argosClean, which="TS")
if (!interactive()) dev.off()

