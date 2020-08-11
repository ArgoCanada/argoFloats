library(argoFloats)
data("index")
index1 <- subset(index, id='1901584')
index2 <- subset(index1, cycle='124') 
profiles2 <- getProfiles(index2)
argos2 <- readProfiles(profiles2)
showQCTests(argos2[[1]], style='full')



