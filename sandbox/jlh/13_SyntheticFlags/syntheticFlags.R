library(argoFloats)
ais <- getIndex(filename = 'synthetic', age=0)
sub <- subset(ais, mode='delayed')
lonRect <- c(54, 70)
latRect <- c(21,23)
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
    print(table(oxygenFlags))
    salinityFlags <- as.vector(argo[["salinityFlag"]])
    print(table(salinityFlags))
    temperatureFlags <- as.vector(argo[["temperatureFlag"]])
    print(table(temperatureFlags))
}
# ~/data/argo/SD2902123_151.nc 

