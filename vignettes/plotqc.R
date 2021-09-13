library(argoFloats)
data("index")
subset <- subset(index, ID='1901584')
profiles <- getProfiles(subset)
argos <- readProfiles(profiles)

if (!interactive()) png("plotqc.png", unit="in", width=7, height=4, pointsize=11, res=100)
oldpar <- par(no.readonly=TRUE)
par((mar=c(3, 2.5, 1, 1)+0.2), mgp=c(2, 0.7, 0))
plot(argos, which='QC', parameter='temperature')
par(oldpar)
if (!interactive()) dev.off()

