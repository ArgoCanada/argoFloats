library(argoFloats)
## source("../../../R/subset.R")
if (!exists("i"))                      # cache for interative tests
    i <- getIndex()
# A first id, then cycle
a1 <- subset(i, id="3900407")
a2 <- subset(a1, cycle='188')
a2[["file"]]

# B first cycle, then id
b1 <- subset(i, cycle='188') # wow, this is very slow
b2 <- subset(b1, id="3900407")
b2[["file"]]
