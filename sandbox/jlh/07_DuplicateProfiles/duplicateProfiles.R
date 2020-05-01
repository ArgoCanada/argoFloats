library(argoFloats)
ai <- getIndex()
l <- length (ai)
set.seed(1) # First sample
s1 <- sample(ai[['index']], 10)
p1 <- getProfiles(s1)
