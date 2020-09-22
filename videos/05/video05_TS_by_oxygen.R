## 1. Jaimie Harbin

library(oce)
library(argoFloats)

# Acquire synthetic argo index, subset by circle, and then subset by oxygen
ai <- getIndex(filename='synthetic')
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


# 2. Dan Kelley

# 2.1 Pressure profile, simple method

pressure <- unlist(argos[["pressure"]])
oxygen <- unlist(argos[["oxygen"]])
plot(oxygen, pressure, ylim=rev(range(pressure, na.rm=TRUE)))

# 2.2 Pressure profile, more sophisticated method
ctd <- as.ctd(unlist(argos[["salinity"]]),
              unlist(argos[["temperature"]]),
              unlist(argos[["pressure"]]))
ctd <- oceSetData(ctd, "oxygen", O2, unit=list(unit=expression(mu*mol/kg), scale=""))
plotProfile(ctd, xtype="oxygen", type="p")

# 2.3 Oxygen in the context of other properties
par(mfrow=c(2, 2))
cex <- 0.5
pch <- 20
col <- rgb(0, 0, 0, alpha=0.1)
ylim <- c(1300, 0)
plotProfile(ctd, xtype="oxygen", ylim=ylim, type="p", pch=pch, col=col, cex=cex, yaxs="i")
plotProfile(ctd, xtype="temperature", ylim=ylim, type="p", pch=pch, col=col, cex=cex, yaxs="i")
plotProfile(ctd, xtype="salinity", ylim=ylim, type="p", pch=pch, col=col, cex=cex, yaxs="i")
plotProfile(ctd, xtype="sigma1", ylim=ylim, type="p", pch=pch, col=col, cex=cex, yaxs="i")

