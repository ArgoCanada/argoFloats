library(argoFloats)
if (!exists("argos")) {
    index1 <- subset(getIndex(), circle=list(latitude=12.25, longitude=64.33, radius=40))
    argos <- readProfiles(getProfiles(index1))
}
plot(argos, which="summary",
     summaryControl=list(items=c("dataStateIndicator","length","longitude","latitude")))

