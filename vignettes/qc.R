## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment="#>")

## ---- warning=FALSE, error=FALSE, message=FALSE, eval=FALSE-------------------
#  library(argoFloats)
#  data("index")
#  index1 <- subset(index, id='1901584')
#  profiles <- getProfiles(index1)
#  argos <- readProfiles(profiles)
#  plot(argos, which='QC', parameter='temperature')

## ---- message=FALSE, error=FALSE, warning=FALSE, eval=FALSE-------------------
#  index2 <- subset(index1, cycle='124')
#  profiles2 <- getProfiles(index2)
#  argos2 <- readProfiles(profiles2)
#  showQCTests(argos2[[1]])

## ---- eval=FALSE--------------------------------------------------------------
#  A <- argos[[1]]
#  D <- data.frame(T = A[['temperature']], TF=A[['temperatureFlag']], S=A[['salinity']], SF=A[['salinityFlag']], P=A[['pressure']], PF=A[['pressureFlag']])

## ---- message=FALSE, error=FALSE, warning=FALSE, eval=FALSE-------------------
#  clean <- applyQC(argos)
#  par(mfrow=c(1, 2))
#  plot(argos, which="TS")
#  plot(clean, which="TS")

## ---- echo=FALSE, warning=FALSE, message=FALSE, error=FALSE, fig.cap="*Figure 5.* Comparison of raw and adjusted oxygen profiles for built-in float file `SD5903586_001.nc`."----
library(argoFloats)
raw <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"))
adjusted <- useAdjusted(raw)
rawOxygen <- unlist(raw[['oxygen']])
rawPressure <- unlist(raw[['pressure']])
adjustedOxygen <- unlist(adjusted[['oxygen']])
adjustedPressure <- unlist(adjusted[['pressure']])
plot(rawOxygen, rawPressure, ylim=rev(range(rawPressure, na.rm=TRUE)), pch=16, col='blue', xlab=expression("Raw Oxygen ["*mu*mol/kg*"]"), ylab='Pressure (dbar)')
points(adjustedOxygen, adjustedPressure, ylim=rev(range(unlist(adjusted[['pressure']]), na.rm=TRUE)), pch=16, col='red')
legend("bottomright", col=c('blue','red'), c("Raw", "Adjusted"), pch=c(16, 16))

## ---- warning=FALSE, error=FALSE, message=FALSE, fig.cap="*Figure 6.* Comparison of raw and adjusted oxygen for built-in float file `SD5903586_001.nc`.  The dotted line is a 1:1 relationship, and the red line is the result of linear regression (see text)."----
plot(rawOxygen, adjustedOxygen,
     xlab=expression("Raw Oxygen ["*mu*mol/kg*"]"),
     ylab=expression("Adjusted Oxygen ["*mu*mol/kg*"]"))
abline(0, 1, lty=3)
model <- lm(adjustedOxygen ~ rawOxygen)
abline(model, col=2)

