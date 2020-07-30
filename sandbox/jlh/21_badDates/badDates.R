library(argoFloats)
library(ncdf4)
i <- argoFloats::getIndex()
flat <- i[["latitude"]]
flon <- i[["longitude"]]
fdate <- i[["date"]]
percentBad <- 100 * sum(!is.finite(fdate)) / length(fdate)
cat(percentBad, "percent of the dates decoded to NA\n")
cat("sampling of bad cases:\n")
badSampling <- head(which(!is.finite(fdate)))
print(i@data$index[badSampling,])
summary(subset(i, badSampling[1]))
p <- readProfiles(getProfiles(subset(i, badSampling[1])))
summary(p[[1]])
file <- nc_open("/Users/jaimiekeeping/data/argo/D1900167_042.nc")

#Jaimie
subset <- subset(getIndex(), cycle='042')
subset2 <- subset(subset, ID='1900167')
sort(names(subset2[['index']])) # vs sort(names(s[['index']])) (which is from index, and has a date)
gp <- getProfiles(subset2)
rp <- readProfiles(gp)
