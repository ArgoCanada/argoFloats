library(oce)
library(testthat)
data(topoWorld) # for checking for consistency, after updates
data(coastlineWorldFine, package="ocedata")
topoWorldOld <- topoWorld
res <- 15

topoFile <- download.topo(west=-180, east=180, south=-90, north=90,
    resolution=res, format="netcdf", destdir=".")

topoWorld <- read.topo(topoFile, debug=5)

if (!interactive()) png("create_topoWorld.png", unit="in",
    res=150, width=7, height=7, pointsize=9)
par(mfrow=c(2, 1), mar=c(1,1,1,1), mgp=c(2,0.7,0))
xlim <- c(-70, -30)
ylim <- c(30, 60)
cm <- colormap(zlim=c(-5000,0), col=oceColorsGebco)
imagep(topoWorldOld, xlim=xlim, ylim=ylim, colormap=cm, xlab="", ylab="")
lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
mtext("oce::topoWorld (30-minute resolution)")
imagep(topoWorld, xlim=xlim, ylim=ylim, colormap=cm, xlab="", ylab="")
lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
mtext(paste0("new topo (", res, "-minute resolution)"))
if (!interactive()) dev.off()

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("topoWorld")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(topoWorld, file="topoWorld.rda", version=2)
    tools::resaveRdaFiles('topoWorld.rda', version=2)
} else {
    save(topoWorld, file="topoWorld.rda")
    tools::resaveRdaFiles('topoWorld.rda')
}


