library(argoFloats)
index <- getIndex(filename='synthetic', age=0)
indexR <- subset(index, mode='realtime')
indexD <- subset(index, mode='delayed')
# Subset in Indian Ocean (OMZ)
lonRect <- c(60,65)
latRect <- c(10,15)
subsetR <- subset(indexR, rectangle=list(longitude=lonRect, latitude=latRect)) # Realtime
subsetD <- subset(indexD, rectangle=list(longitude=lonRect, latitude=latRect)) # Delayed
#Real time
profilesR <- getProfiles(subsetR)
argosR <- readProfiles(profilesR)
oxygenR <- unlist(argosR[['oxygen']])
finiteR <- 100*(sum(is.finite(oxygenR))/length(oxygenR))
finiteR ## 13% are real values
# Delayed
profilesD <- getProfiles(subsetD)
argosD <- readProfiles(profilesD)
oxygenD <- unlist(argosD[['oxygen']])
finiteD <- 100*(sum(is.finite(oxygenD))/length(oxygenD))
finiteD ## ~ 66 % are real values
