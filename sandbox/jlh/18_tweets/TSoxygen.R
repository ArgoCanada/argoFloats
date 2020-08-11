library(oce)
library(argoFloats)
if (!interactive()) png("TSoxygen.png", unit="in", width=7, height=4, pointsize=11, res=100)
par(mar=c(3,3,1,1))
# Acquire merged argo index, subset by circle, and then subset by oxygen
ai <- getIndex(filename='merge')
subc <- subset(ai, circle=list(longitude=-77.5, latitude=27.5, radius=300))
subo <- subset(subc, parameter='DOXY')

# Download argo files, read them, and thn extract O2
profiles <- getProfiles(subo)
argos <- readProfiles(profiles)
O2 <- unlist(argos[["oxygen"]]) # note that [[ returns a list

# Plot colour palette, and then the TS plot
cm <- colormap(O2)
drawPalette(colormap=cm, zlab="Oxygen", pos=3)
plot(argos, which="TS", col=cm$zcol)
if (!interactive()) dev.off()