library(argoFloats)
ai <- getIndex()
from <- as.POSIXct("2020-01-01", tz="UTC")
to <- as.POSIXct("2020-01-31", tz="UTC")
sub <- subset(ai, time=list(from=from, to=to))
s4 <- subset(sub, 1:4)
s4[['file']]
