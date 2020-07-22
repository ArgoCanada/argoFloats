library(argoFloats)
subset <- subset(getIndex(), deep=TRUE)
sub2 <- subset(subset, circle=list(longitude=105, latitude=-67, radius=800))
