if (!interactive()) png("readProfiles.png", unit="in", width=5, height=2.8, pointsize=11, res=100)
par((mar=c(3, 2.5, 1, 1)+0.1), mgp=c(2, 0.7, 0), cex.lab=0.9)
library(argoFloats)
data(index)
sub <- subset(index, 1:2) # To subset for profiles
profiles <- getProfiles(sub)
argos <- readProfiles(profiles)
argosClean <- applyQC(argos)
pressure <- argosClean[['argos']][[1]][['pressure']]
temperature <- argosClean[['argos']][[1]][['temperature']]
plot(temperature, pressure, ylim=rev(range(pressure, na.rm=TRUE)),
     xlab='Temperature (C)', ylab='Depth (dbar)')
if (!interactive()) dev.off()