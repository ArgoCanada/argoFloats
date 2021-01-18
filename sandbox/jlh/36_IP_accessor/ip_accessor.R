library(argoFloats)
data(index)
index1 <- subset(index, 1:10)
argos <- readProfiles(getProfiles(index1))
for (i in seq_along(argos[["argos"]])) {
    historyList <- argos[["historyAction"]][[i]][1,]
}
historyList <- lapply(argos[["historyAction"]], function(h) argos[["historyAction"]][[i]][1,])
keeph <- grepl("IP", historyList)
index1@data$index <- index1@data$index[keeph, ]

