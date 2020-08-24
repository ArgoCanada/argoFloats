# QC Plot
library(argoFloats)
data("index")
index1 <- subset(index, id='1901584')
profiles <- getProfiles(index1)
argos <- readProfiles(profiles)
clean <- applyQC(argos)
plot(clean, which="TS")

str(clean[['temperature']])
clean[['cycle']]
vec <- c(125,126)
lapply(vec, function(v)  which(v ==clean[['cycle']]))