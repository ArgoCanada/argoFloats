library(argoFloats)
if (!exists("ai"))
       ai <- getIndex()
index1 <- subset(ai, circle=list(longitude=-60.85,latitude=46.24, radius=200)) # Canada
index2 <- subset(ai, circle=list(latitude=59.33, longitude=18.06, radius=100)) # Sweeden
merge <- merge(index1,index2)
plot(merge)
argos1 <- readProfiles(getProfiles(index1))
argos2 <- readProfiles(getProfiles(index2))
par(mfrow=c(1,2))
plot(argos1, which="profile", profileControl=list(parameter="temperature"),type="p", sub="Nova Scotia, Canada")
plot(argos2, which="profile", profileControl=list(parameter="temperature"), type="p", sub="Stockholm, Sweeden")
