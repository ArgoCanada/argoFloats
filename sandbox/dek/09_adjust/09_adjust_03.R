if (FALSE) { # slow ... no longer needed, since in 04 we know a good filenam
    ## Look for changed adjusted values in whole indexSynthetic database
    library(argoFloats)
    data(indexSynthetic)
    s <- subset(index, 1:500) # failed downloads somewhere in the 600s, so look in first 500
    p <- getProfiles(s)
    a <- readProfiles(p)
    field <- "DOXY"
    for (i in seq_len(a[["length"]])) {
        argo <- a[[i]]
        same <- all.equal(argo[[field]], argo[[paste0(field, "_ADJUSTED")]])
        cat("i=", i, ", file='", argo[["filename"]], "', DOXY=DOXY_ADJUSTED=", same, "\n", sep="")
        if (!same)
            cat("   avg ", field, "=", mean(argo[[field]], na.rm=TRUE), ", ", field, "_AJUSTED=", mean(argo[[paste0(field, "_ADJUSTED")]]), "\n", sep="")
    }
}
