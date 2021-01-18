library(argoFloats)
data(index)
index1 <- subset(index, 1:10)
argos <- readProfiles(getProfiles(index1))
for (i in seq_along(argos)) {
history <- unlist(argos[["historyAction"]][[i]])
keep <- grepl("IP", history)
print(keep)
}
x@data$index <- x@data$index[keep, ]
