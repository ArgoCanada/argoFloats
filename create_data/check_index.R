library(oce)
library(argoFloats)
data(coastlineWorldFine, package="ocedata")
data(topoWorld, package="oce")
load("index.rda")
par(mar=c(2, 2, 1, 1))
mapPlot(coastlineWorldFine, col="tan",
        latitudelim=c(40, 50), longitudelim=c(-70, -50),
        projection="+proj=merc +lon_0=-60")
mapPoints(index[["longitude"]], index[["latitude"]], pch=20, col=rgb(1,0,0,0.1))
mtext(paste(length(index[["longitude"]]), "Argo profiles in data(index)"))
mapContour(topoWorld[["longitude"]], topoWorld[["latitude"]], -topoWorld[["z"]],
           levels=c(1000, 2000), col="blue", lty=c(2,1))
legend("topleft", col="blue", lty=c(2,1), title="Water Depth [m]", legend=c("1000", "2000"),
       bg="white")

