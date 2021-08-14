library(argoFloats)
raw <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"))
adjusted <- useAdjusted(raw)
rawOxygen <- unlist(raw[['oxygen']])
rawPressure <- unlist(raw[['pressure']])
adjustedOxygen <- unlist(adjusted[['oxygen']])
adjustedPressure <- unlist(adjusted[['pressure']])
xlim <- range(c(rawOxygen, adjustedOxygen), na.rm=TRUE)
plot(rawOxygen, rawPressure,
     pch=1, col=1,
     xlim=xlim, ylim=rev(range(rawPressure, na.rm=TRUE)),
     xlab=expression("Raw Oxygen ["*mu*mol/kg*"]"),
     ylab='Pressure (dbar)')
points(adjustedOxygen, adjustedPressure,
       pch=3, col=2)
legend("bottomright", pch=c(1,3), col=c(1,2), c("Raw", "Adjusted"))

