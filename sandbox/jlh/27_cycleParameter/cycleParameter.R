# QC Plot
library(argoFloats)
data("index")
index1 <- subset(index, id='1901584')
profiles <- getProfiles(index1)
argos <- readProfiles(profiles)
clean <- applyQC(argos)
plot(clean, which="TS")

C <-clean[['cycle']]
for(i in C) {
    subset <- subset(clean, cycle=i)
    plot(subset, which='TS')
}

str(clean[['temperature']])
clean[['cycle']]
C <-as.numeric((clean[['cycle']]))
lapply(range, function(v)  which(v ==clean[['cycle']]))
