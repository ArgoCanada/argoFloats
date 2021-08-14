## Video 8: Introduction to Quality Control

# QC Plot
library(argoFloats)
data("index")
index1 <- subset(index, ID='1901584')
profiles <- getProfiles(index1)
argos <- readProfiles(profiles)
plot(argos, which='QC', parameter='temperature')

# showQCTests()
index1[['cycle']]
index2 <- subset(index1, cycle='124')
profiles2 <- getProfiles(index2)
argos2 <- readProfiles(profiles2)
showQCTests(argos2[[1]])
#showQCTests(argos2[[1]], style='full')

# applyQC()
clean <- applyQC(argos)
par(mfrow=c(1, 2))
plot(argos, which="TS")
plot(clean, which="TS")

