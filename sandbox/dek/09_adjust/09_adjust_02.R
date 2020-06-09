library(argoFloats)
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
    cat(argo[["filename"]], " hypothesis is ", if (hypothesis) "TRUE\n" else "FALSE\n", sep="")
    cat("    mean temperature:           ", mean(argo[["temperature"]], na.rm=TRUE), "\n")
    cat("    mean temperatureUnadjusted: ", mean(argo[["temperatureUnadjusted"]], na.rm=TRUE), "\n")
}
table(testOfHypothesis)
bad <- which(!testOfHypothesis)
cat("Hypothesis failed on cases: ", paste(which(!testOfHypothesis), collapse=" "), "\n")
cat("First failed test is case #", bad, ", which has following summary:\n", sep="")
summary(a[[bad[1]]])

