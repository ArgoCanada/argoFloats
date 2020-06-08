library(argoFloats)
library(oce)
library(ocedata)
data(coastlineWorldMedium)
# Subsetting for the Arabian Sea (Oxygen Minimum Zone)
if(!exists("ai")) {
    ai <- getIndex()
    realtime <- subset(ai, mode='realtime')
    delayed <- subset(ai , mode='delayed')
    lonRect <- c(54, 70)
    latRect <- c(21,23)
    rt <- subset(realtime, rectangle=list(longitude=lonRect, latitude=latRect))
    dt <- subset(delayed, rectangle=list(longitude=lonRect, latitude=latRect))
}
#Real time
unique <- unique(rt[['ID']])
IDr <- sort(unique)
length(IDr)

#Delayed
unique <- unique(dt[['ID']])
IDd <- sort(unique)
length(IDd)

#Checking if delayed IDs exist within realtime for Arabian Sea subset.
for (ID in IDd) {
    ss <- subset(realtime, ID='IDd')
}
