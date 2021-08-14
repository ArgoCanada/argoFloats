# A multi-column BGC case
library(argoFloats)
if (FALSE) { # use locally-downloaded for reproducibility
    i <- getIndex("bgc")
    s <- subset(i, ID="6901494")
    s <- subset(s, cycle=352)              # 352 and 352D
}
a <- readProfiles(c("BD6901494_352.nc",  "BD6901494_352D.nc"))
if (FALSE) { # sanity check (used to guide code, no longer needed)
    a1 <- a[[1]]                           # focus on 352
    stopifnot(all.equal(ncol(a1[["pressure"]]), length(a1[["parameterDataMode"]])))
    a1[["parameterDataMode"]]
    a1[["dataMode"]]
    a1[["parameter"]]
}

aAdj1 <- useAdjusted(a, debug=2)
aAdj2 <- useAdjusted(a, fallback=TRUE, debug=2)

