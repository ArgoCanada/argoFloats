library(argoFloats)
data(index)
s <- subset(index, 1:20)
p <- getProfiles(s)
a <- readProfiles(p)
cat("Demonstration: adjusted data are not always good!\n")
for (argo in a[["profile"]]) {
    cat(argo[["filename"]], "\n")
    cat("  mean temperature:           ", mean(argo[["temperature"]], na.rm=TRUE), "\n")
    cat("  mean temperatureUnadjusted: ", mean(argo[["temperatureUnadjusted"]], na.rm=TRUE), "\n")
}


