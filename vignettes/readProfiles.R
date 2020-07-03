if (!interactive()) png("readProfiles.png", unit="in", width=5, height=2.8, pointsize=11, res=150)
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