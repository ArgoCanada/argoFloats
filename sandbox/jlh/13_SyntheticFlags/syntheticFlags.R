library(argoFloats)
ais <- getIndex(filename = 'synthetic')
sub <- subset(ais, mode='delayed')
lonRect <- c(-77, -75)
latRect <- c(25, 28)
s <- subset(sub, rectangle=list(longitude=lonRect, latitude=latRect))
profiles <- getProfiles(s)
argos <- readProfiles(profiles)

n <- profiles[["length"]]
files <- profiles[["file"]]
for (i in seq_len(n)) {
    argo <- argos[[i]]
    cat(rep("-", length.out=30), "\n", sep="")
    cat(files[i], "\n")
    cat(rep("-", length.out=30), "\n", sep="")
    oxygenFlags <- as.vector(argo[["oxygenFlag"]])
    print(table(oxygenFlags)) # For table(oxygenFlags[1], there is 115 values 1)
    salinityFlags <- as.vector(argo[["salinityFlag"]])
    print(table(salinityFlags))
    temperatureFlags <- as.vector(argo[["temperatureFlag"]])
    print(table(temperatureFlags))
}



stop()
oxygenFlags <- argos[[1]]@metadata$flags$oxygen
temperatureFlags <- argos[[1]]@metadata$flags$temperature
salinityFlags <- argos[[1]]@metadata$flags$salinity

