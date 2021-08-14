# Demonstrating [["dataStateIndicator"]] near Cape Breton
library(argoFloats)
index1 <- subset(getIndex(), circle=list(latitude=46.24, longitude=-60.85, radius=400))
# Determining amount of samples per ID
id <- index1[["id"]]
tableSorted <- sort(table(id)) # skipped 4901789 to have same number of cycles and near the same area
index2 <- subset(index1, ID="4902467")
index3 <- subset(index1, ID="3901642")
argos2 <- readProfiles(getProfiles(index2))
argos3 <- readProfiles(getProfiles(index3))
argos2[['dataStateIndicator']]
argos3[['dataStateIndicator']]

## NEW (Arabian Sea - OMZ)
library(argoFloats)
index1 <- subset(getIndex(), circle=list(latitude=12.25, longitude=64.33, radius=40))
argos <- readProfiles(getProfiles(index1))
indicator <- c("3C", "3B", "2C+", "2C","2B+", "2B", "1A", "0A")
for (i in 1:8) {
         argos2 <- subset(argos, dataStateIndicator=indicator[i])
         print(length(argos2[['filename']]))
}
