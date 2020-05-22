library(argoFloats)
data(index)
i <- subset(index, 1:5)
i[[1]]
p <- getProfiles(i)
p[[1]]
a <- readProfiles(p)
summary(a[[1]])
