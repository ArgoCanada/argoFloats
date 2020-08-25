# QC Plot
library(argoFloats)
data("index")
index1 <- subset(index, id='1901584')
profiles <- getProfiles(index1)
argos <- readProfiles(profiles)
clean <- applyQC(argos)
plot(clean, which="TS")

C <-as.numeric(index1[['cycle']])
for(i in C) {
    subset <- subset(index1, cycle=i)
    profiles <- getProfiles(subset)
    argos <- readProfiles(profiles)
    plot(argos, which='TS')
}
    


#str(clean[['temperature']])
#clean[['cycle']]
#C <-as.numeric((clean[['cycle']]))
#lapply(range, function(v)  which(v ==clean[['cycle']]))
