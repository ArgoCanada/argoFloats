png("exampleTS.png", width=5, height=3, unit="in", res=120, pointsize=10)
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
dev.off()

