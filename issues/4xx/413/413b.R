library(argoFloats)
set.seed(413)
i <- getIndex()
len <- i[["length"]]
ntrials <- 2 # 200
s <- subset(i, sample(seq_len(len), ntrials))
raw <- readProfiles(getProfiles(s))
adj <- useAdjusted(raw, fallback=TRUE, debug=2)

