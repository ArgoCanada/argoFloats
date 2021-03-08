library(argoFloats)
set.seed(413)
i <- getIndex()
len <- i[["length"]]
ntrials <- 200
s <- subset(i, sample(seq_len(len), ntrials))
raw <- readProfiles(getProfiles(s))
#> source("~/git/argoFloats/R/adjusted.R")
adj <- useAdjusted(raw, fallback="raw", debug=2)

