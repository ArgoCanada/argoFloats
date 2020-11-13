library(argoFloats)
## Arabian Sea
index1 <- subset(getIndex(), circle=list(latitude=12.25, longitude=64.33, radius=40))
argos1 <- readProfiles(getProfiles(index1))
## Per-profile tabulation
table(unlist(argos1[["dataStateIndicator"]]))
## Get and test subset
argos2 <- subset(argos1, dataStateIndicator="2C")
argos2[["dataStateIndicator"]]
