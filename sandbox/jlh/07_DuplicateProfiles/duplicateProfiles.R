library(argoFloats)
ai <- getIndex()
sub <- subset(ai, 1:100)
profiles <- getProfiles(sub)
argos <- readProfiles(profiles)
argos