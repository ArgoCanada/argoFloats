library(argoFloats)
source("../../../R/subset.R")
if (!exists("i"))                      # cache for interative tests
    i <- getIndex()
# A first ID, then profile
a1 <- subset(i, ID="3900407")
a2 <- subset(a1, profile='188')
a2[["file"]]

# B first profile, then ID
b1 <- subset(i, profile='188') # wow, this is very slow
b2 <- subset(b1, ID="3900407")
b2[["file"]]
