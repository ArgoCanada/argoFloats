library(argoFloats)
#Index (core Argo)
data(index)
set.seed(1) # First sample
n <- 50
s <- subset(index, sample(seq_along(index[['longitude']]), n))
p <- getProfiles(s)
a <- readProfiles(p, adjusted=TRUE)
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
stop()

# BGC Argo
data(indexBgc)
pb <- getProfiles(indexBgc)
ab <- readProfiles(pb, adjusted=TRUE)
cat("Demonstration: adjusted data are not always good!\n")
testOfHypothesis <- NULL
for (argo in ab[["profile"]]) {
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

# Synthetic
data("indexSynthetic")
ps <- getProfiles(indexSynthetic)
as <- readProfiles(ps, adjusted=TRUE)
cat("Demonstration: adjusted data are not always good!\n")
testOfHypothesis <- NULL
for (argo in as[["profile"]]) {
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

# Merged
data("indexMerged")
pm <- getProfiles(indexSynthetic)
am <- readProfiles(pm, adjusted=TRUE)
cat("Demonstration: adjusted data are not always good!\n")
testOfHypothesis <- NULL
for (argo in argos[["profile"]]) {
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


