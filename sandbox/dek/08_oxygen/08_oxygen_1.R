library(argoFloats)
library(oce)
if (!exists("argoMerged")) { # cache for speed of repeated interactive source() actions
    data("indexMerged") # 39 items
    profilesMerged <- getProfiles(indexMerged)
    argoMerged <- readProfiles(profilesMerged)
}
if (!exists("argoSynthetic")) { # cache for speed of repeated interactive source() actions
    data("indexSynthetic") # 39 items
    profilesSynthetic <- getProfiles(indexSynthetic)
    argoSynthetic <- readProfiles(profilesSynthetic)
}

n <- argoMerged[["length"]] # number of profiles

if (!interactive()) pdf("08_oxygen_1.pdf")
for (i in seq_len(n)) {
    ## Merged profile
    filename <- profilesMerged[["file"]][i]
    summary(argoMerged[[i]])
    ctdMerged <- as.ctd(argoMerged[[i]])

    ## Synthetic profile
    summary(argoSynthetic[[i]])
    ctdSynthetic <- as.ctd(argoSynthetic[[i]])

    ## show that profiles are identical
    cat("i=",i, ", filename=", filename, "\n")
    haveOxygen <- any(is.finite(ctdMerged[["oxygen"]]))
    if (haveOxygen) {
        maxdiff <- max(abs(ctdMerged[["oxygen"]] - ctdSynthetic[["oxygen"]]), na.rm=TRUE)
        plotProfile(ctdMerged, xtype="oxygen")
        lines(ctdSynthetic[["oxygen"]], ctdSynthetic[["pressure"]], col=2, lty="dotted")
        mtext(paste0(filename, " (max non-NA oxygen diff=", maxdiff, ")"))
    } else {
        plot(0:1, 0:1, xlab="", ylab="", type="n")
        box()
        mtext(paste(filename, "has no oxygen data"))
    }
}
if (!interactive()) dev.off()

## > grep("^[a-z]*/[0-9]*/profiles/.{0,1}R.*$", indexMerged[["file"]])
## [1] 34 35
realtime <- grep("^[a-z]*/[0-9]*/profiles/.{0,1}R.*$", index[["file"]])
delayed <- grep("^[a-z]*/[0-9]*/profiles/.{0,1}D.*$", index[["file"]])
length(realtime)
length(delayed)
