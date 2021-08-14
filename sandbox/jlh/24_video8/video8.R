## Video 8: Quality Control

## Part 1: Overview

# QC Plot
library(argoFloats)
data("index")
index1 <- subset(index, id='1901584')
profiles <- getProfiles(index1)
argos <- readProfiles(profiles)
plot(argos, which='QC', parameter='temperature')

# showQCTests
index1[['cycle']]
index2 <- subset(index1, cycle='124')
profiles2 <- getProfiles(index2)
argos2 <- readProfiles(profiles2)
showQCTests(argos2[[1]])
#showQCTests(argos2[[1]], style='full')

# applyQC()
clean <- applyQC(argos)
par(mfrow=c(1, 2))
plot(argos, which="TS")
plot(clean, which="TS")


## Part 2: Exploring bad Data

# Identify the bad data
par(mfrow=c(1,1))
plot(clean, which="TS", eos="gsw")
SA <- unlist(clean[["SA"]])
CT <- unlist(clean[["CT"]])
bad <- identify(SA, CT, n=1)
SAbad <- SA[bad]
CTbad <- CT[bad]
points(SAbad, CTbad) # plot it, to check

# Find the cycle that holds the identified bad data
cycles <- clean[["argos"]]
for (icycle in seq_along(cycles)) {
    cycle <- cycles[[icycle]]
    w <- which(SAbad == cycle[["SA"]] & CTbad == cycle[["CT"]])
    if (length(w)) {
        message("found bad point SA=", SAbad, ", CT=", CTbad,
                " in cycle ", icycle, " level ", w,
                " pressure ", cycle[["pressure"]][w])
        break
    }
}

