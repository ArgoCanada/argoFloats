library(argoFloats)
data(index)
set.seed(1) # First sample
n <- 50
s <- subset(index, sample(seq_along(index[['longitude']]), n))
p <- getProfiles(s)
a <- useAdjusted(readProfiles(p))
cat("Demonstration: adjusted data are not always good!\n")
testOfHypothesis <- NULL
for (i in seq_len(a[["length"]])) {
    argo <- a[[i]]
    isRealtime <- grepl("/[SM]{0,1}R", argo[["filename"]])
    allNA <- all(!is.finite(argo[["temperature"]]))
    hypothesis <- if (isRealtime) allNA else TRUE
    testOfHypothesis <- c(testOfHypothesis, hypothesis)
    cat(sprintf("%2d %s isRealtime=%s allNA=%s hypothesis=%s dataMode=%s (== isRealtime: %s)\n",
                i,
                gsub(".*/data/argo/", "", argo[["filename"]]),
                isRealtime,
                allNA,
                hypothesis,
                paste(argo[["dataMode"]], collapse=","),
                isRealtime == (argo[["dataMode"]][1]=="A")))
    cat(sprintf("   T: %6.3f unadjusted: %s\n",
                mean(argo[["temperature"]], na.rm=TRUE),
                if ("temperatureUnadjusted" %in% names(argo@data))
                    sprintf("%6.3f", mean(argo[["temperatureUnadjusted"]], na.rm=TRUE)) else "(missing"))
}
dataMode <- unlist(lapply(a[["argos"]], function(x) x[["dataMode"]][1]))
cat("\n\n")
cat("Hypothesis worked on cases: ", paste(which(testOfHypothesis), collapse=" "), "\n", sep="")
cat("datamode=='D'     on cases: ", paste(which(dataMode=="D"), collapse=" "), "\n", sep="")
cat("Hypothesis failed on cases: ", paste(which(!testOfHypothesis), collapse=" "), "\n", sep="")
cat("datamode=='A'     on cases: ", paste(which(dataMode=="A"), collapse=" "), "\n", sep="")

bad <- which(!testOfHypothesis)
good <- which(testOfHypothesis)

badExample <- a[[bad[1]]]
cat("\n\nFirst failed test is case #", bad[1], ", which has following summary:\n", sep="")
summary(badExample)

goodExample <- a[[good[1]]]
cat("\n\nFirst good test is case #", good[1], ", which has following summary:\n", sep="")
summary(goodExample)

