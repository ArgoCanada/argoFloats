# QC Plot
library(argoFloats)
data("index")
index1 <- subset(index, id='1901584')
profiles <- getProfiles(index1)
argos <- readProfiles(profiles)
plot(argos, which='QC', parameter='temperature')

# showQCTests
index1[['cycle']]
index2 <- subset(index1, cycle='124') 
profiles2 <- getProfiles(index2)
argos2 <- readProfiles(profiles2)
showQCTests(argos2[[1]], style='full')

# applyQC()
clean <- applyQC(argos)
par(mfrow=c(1, 2))
plot(argos, which="TS")
plot(clean, which="TS")

# Exploring bad Data
# left point
par(mfrow=c(1, 1))
T <- unlist(clean[['temperature']])
S <- unlist(clean[['salinity']])
plot(S,T)
identify(S,T)
# To determine which profile the 467th position is in
for (x in 1:9){
    print(length(unlist(clean[['salinity']][x])))
}
# This tells us is in the 7th cycle at position 54.
S[467]
argos[['salinity']][[7]][54,]
T[467]
argos[['temperature']][[7]][54,]
salinityF <- unlist(clean[['salinityFlag']])[467]
temperatureF <- unlist(clean[['temperatureFlag']])[467]

# Right points
identity(S,T)
salinityFR <- unlist(clean[['salinityFlag']])[546]
temperatureFR <- unlist(clean[['temperatureFlag']])[546]





