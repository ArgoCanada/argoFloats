png("exampleTS.png", width=7, height=5, unit="in", res=120, pointsize=14)
library(argoFloats)
library(oce)
# TS diagram for profiles near Abaco Island, Bahamas.
indexAll <- getIndex(destdir="~/data/argo")
index <- subset(indexAll,
                circle=list(longitude=-77.06,latitude=26.54,radius=30))
profileIndex <- getProfiles(index)
argos <- readProfiles(profileIndex)
first <- TRUE
# Semi-transparent colours hint at sample repeatability
col <- rgb(0, 0, 1, 0.4)
for (i in seq_len(argos[["profile count"]])) {
    argo <- handleFlags(argos[["profile", i]])
    # Skip profiles which lack no valid salinities
    ok <- sum(is.finite((argo[["salinity"]])))
    if (ok > 0) {
        if (first) {
            plotTS(argo, Slim=c(35, 37), Tlim=c(3, 30), eos="gsw",
                   cex=0.5, col=col, pch=20)
            first <- FALSE
        } else {
            points(argo[["SA"]], argo[["CT"]],
                   cex=0.5, col=col, pch=20)
        }
    }
}
dev.off()

