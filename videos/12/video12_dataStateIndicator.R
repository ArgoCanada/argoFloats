## Note current code doesn't work. Need to fix subsetting by dataStateIndicator
## and handle more than one column in summary plot

# Jaimie 

library(argoFloats)
index1 <- subset(getIndex(), circle=list(latitude=12.25, longitude=64.33, radius=40))
argos <- readProfiles(getProfiles(index1))
table(unlist(argos[["dataStateIndicator"]]))
indicator <- c("3C", "3B", "2C+", "2C","2B+", "2B", "1A", "0A")
argos2 <- subset(argos, dataStateIndicator="2C")

# Dan

plot(argos, which="summary",
     summaryControl=list(items=c("dataStateIndicator","length","longitude","latitude")))
