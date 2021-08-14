# Demonstrate subset(argos, dataMode="")

library(argoFloats)

# Case 1: 'delayed' profile
file <- "D4900785_048.nc"
A <- readProfiles(system.file("extdata", file, package="argoFloats"), quiet=TRUE)
stopifnot("D" == A[[1]][["dataMode"]])
# Next retains *all* data
B <- subset(A, dataMode="delayed")
stopifnot(0 < sum(is.finite(B[[1]][["salinity"]])))
# Next retains *no* data
C <- subset(A, dataMode="realtime")
stopifnot(0 == sum(is.finite(C[[1]][["salinity"]])))
# Next retains *no* data
D <- subset(A, dataMode="adjusted")
stopifnot(0 == sum(is.finite(D[[1]][["salinity"]])))

# Case 2: 'adjusted' profile
file <- "R3901602_163.nc"
A <- readProfiles(system.file("extdata", file, package="argoFloats"), quiet=TRUE)
stopifnot("A" == A[[1]][["dataMode"]])
# Next retains *all* data
B <- subset(A, dataMode="adjusted")
stopifnot(0 < sum(is.finite(B[[1]][["salinity"]])))
# Next retains *no* data
C <- subset(A, dataMode="realtime")
stopifnot(0 == sum(is.finite(C[[1]][["salinity"]])))
# Next retains *no* data
D <- subset(A, dataMode="delayed")
stopifnot(0 == sum(is.finite(D[[1]][["salinity"]])))

