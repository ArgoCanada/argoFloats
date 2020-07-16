library(argoFloats)
data(index)
s <- subset(index, 1:50)
p <- getProfiles(s)
a <- useAdjusted(readProfiles(p))
cat("Demonstration: adjusted data are not always good!\n")
testOfHypothesis <- NULL
for (argo in a[["profile"]]) {
    isRealtime <- grepl("/[SM]{0,1}R", argo[["filename"]])
    allNA <- all(!is.finite(argo[["temperature"]]))
    hypothesis <- if (isRealtime) allNA else TRUE
    testOfHypothesis <- c(testOfHypothesis, hypothesis)
    if (!hypothesis) {
        cat(argo[["filename"]], "\n", sep="")
        cat("    mean temperature:           ", mean(argo[["temperature"]], na.rm=TRUE), "\n")
        cat("    mean temperatureUnadjusted: ", mean(argo[["temperatureUnadjusted"]], na.rm=TRUE), "\n")
        warning("bad hypothesis on file ", argo[["filename"]], "\n")
    }
}
table(testOfHypothesis)

