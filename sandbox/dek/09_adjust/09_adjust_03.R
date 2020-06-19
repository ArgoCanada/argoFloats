## Look for changed adjusted values in whole indexSynthetic database
library(argoFloats)
data(indexSynthetic)
s <- subset(index, 1:500) # failed downloads somewhere in the 600s, so look in first 500
p <- getProfiles(s)
a <- readProfiles(p)
for (i in seq_len(a[["length"]])) {
    argo <- a[[i]]
    same <- all.equal(argo[["DOXY"]], argo[["DOXY_ADJUSTED"]])
    cat("i=", i, ", file='", argo[["filename"]], "', DOXY=DOXY_ADJUSTED=", same, "\n", sep="")
    if (!same)
        cat("   adv OXY=", mean(argo[["DOXY"]], na.rm=TRUE), ", OXY_AJUSTED=", mean(argo[["DOXY_ADJUSTED"]]), "\n", sep="")
}

