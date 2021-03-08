# A multi-column BGC case
library(argoFloats)
i <- getIndex("bgc")
s <- subset(i, ID="6901494")
s <- subset(s, cycle=352)              # 352 and 352D
a <- readProfiles(getProfiles(s))
a1 <- a[[1]]                           # focus on 352
# sanity check
stopifnot(all.equal(ncol(a1[["pressure"]]), length(a1[["parameterDataMode"]])))
a1[["parameterDataMode"]]
a1[["parameter"]]

