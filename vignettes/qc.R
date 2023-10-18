## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment="#>")

## ----eval=FALSE---------------------------------------------------------------
#  library(argoFloats)
#  load_all()
#  browseVignettes("argoFloats")

## ----echo=FALSE---------------------------------------------------------------
ID <- "1901584"

## ----warning=FALSE, error=FALSE, message=FALSE, eval=FALSE--------------------
#  library(argoFloats)
#  data("index")
#  index1 <- subset(index, ID="1901584")
#  profiles <- getProfiles(index1)
#  argos <- readProfiles(profiles)
#  plot(argos, which="QC", parameter="temperature")

## ----message=FALSE, error=FALSE, warning=FALSE, eval=FALSE--------------------
#  index2 <- subset(index1, cycle="124")
#  profiles2 <- getProfiles(index2)
#  argos2 <- readProfiles(profiles2)
#  showQCTests(argos2[[1]])

## ----eval=FALSE---------------------------------------------------------------
#  A <- argos[[1]]
#  D <- data.frame(T=A[["temperature"]], TF=A[["temperatureFlag"]],
#                  S=A[["salinity"]], SF=A[["salinityFlag"]],
#                  P=A[["pressure"]], PF=A[["pressureFlag"]])

## ----message=FALSE, error=FALSE, warning=FALSE, eval=FALSE--------------------
#  clean <- applyQC(argos)
#  oldpar <- par(no.readonly=TRUE)
#  par(mfrow=c(1, 2))
#  plot(argos, which="TS")
#  plot(clean, which="TS")
#  par(oldpar)

## ----eval=FALSE---------------------------------------------------------------
#  remotes::install_github("dankelley/oce", ref="develop")

## ----warning=FALSE, message=FALSE, error=FALSE, fig.cap="*Figure 5.* Comparison of raw and adjusted oxygen profiles for built-in float file `SD5903586_001.nc`."----
if (packageVersion("oce") > "1.2.0") {
    library(argoFloats)
    raw <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"))
    adjusted <- useAdjusted(raw)
    rawOxygen <- unlist(raw[["oxygen"]])
    rawPressure <- unlist(raw[["pressure"]])
    adjustedOxygen <- unlist(adjusted[["oxygen"]])
    adjustedPressure <- unlist(adjusted[["pressure"]])
    xlim <- range(c(rawOxygen, adjustedOxygen), na.rm=TRUE)
    plot(rawOxygen, rawPressure,
         pch=1, col=1,
         xlim=xlim, ylim=rev(range(rawPressure, na.rm=TRUE)),
         xlab=expression("Raw Oxygen ["*mu*mol/kg*"]"),
         ylab="Pressure (dbar)")
    points(adjustedOxygen, adjustedPressure,
           pch=3, col=2)
    legend("bottomright", pch=c(1,3), col=c(1,2), c("Raw", "Adjusted"))
}

## ----warning=FALSE, error=FALSE, message=FALSE, fig.cap="*Figure 6.* Comparison of raw and adjusted oxygen for built-in float file `SD5903586_001.nc`.  The dotted line is a 1:1 relationship, and the red line is the result of linear regression (see text)."----
if (packageVersion("oce") > "1.2.0") {
    plot(rawOxygen, adjustedOxygen,
         xlab=expression("Raw Oxygen ["*mu*mol/kg*"]"),
         ylab=expression("Adjusted Oxygen ["*mu*mol/kg*"]"))
    abline(0, 1, lty=3)
    model <- lm(adjustedOxygen ~ rawOxygen)
    abline(model, col=2)
}

## ----echo=FALSE---------------------------------------------------------------
if (packageVersion("oce") > "1.2.0") {
    residual <- round(mean(abs((model$residuals))),8)
} else {
    residual <- 9.8e-7 # value obtained with oce 1.3.0
}

## -----------------------------------------------------------------------------
if (packageVersion("oce") > "1.2.0") {
    summary(model)
}

## ----eval=FALSE---------------------------------------------------------------
#  library(argoFloats)
#  data("indexSynthetic")
#  index1 <- subset(indexSynthetic, ID="4900845")
#  profiles <- getProfiles(index1)
#  argos <- readProfiles(profiles)
#  plot(argos, which="QC", parameter="temperature")

## ----eval=FALSE---------------------------------------------------------------
#  library(argoFloats)
#  data("indexSynthetic")
#  index1 <- subset(indexSynthetic, ID="4900845")
#  profiles <- getProfiles(index1)
#  argos <- readProfiles(profiles)
#  a1 <- argos[[1]]
#  showQCTests(a1)

## ----eval=FALSE---------------------------------------------------------------
#  library(argoFloats)
#  # Contrast TS diagrams for raw and flag-handled data
#  data(indexSynthetic)
#  index1 <- subset(indexSynthetic, ID="4900845")
#  argos <- readProfiles(getProfiles(index1))
#  clean <- applyQC(argos)
#  oldpar <- par(no.readonly=TRUE)
#  par(mfrow=c(1, 2))
#  plot(argos, which="TS")
#  plot(clean, which="TS")
#  par(oldpar)

## ----eval=FALSE---------------------------------------------------------------
#  summary(unlist(argos[["temperature"]]))
#  summary(unlist(clean[["temperature"]]))
#  summary(unlist(argos[["salinity"]]))
#  summary(unlist(clean[["salinity"]]))

## ----eval=FALSE---------------------------------------------------------------
#  library(argoFloats)
#  raw <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"))
#  adjusted <- useAdjusted(raw)
#  rawC <- unlist(raw[["chlorophyllA"]])
#  rawPressure <- unlist(raw[["pressure"]])
#  adjustedC <- unlist(adjusted[["chlorophyllA"]])
#  adjustedPressure <- unlist(adjusted[["pressure"]])
#  plot(rawC, rawPressure, ylim=rev(range(rawPressure, na.rm=TRUE)), pch=16, col="blue", xlab=expression("Raw Chlorophyll A (mg/m3)"), ylab="Pressure (dbar)")
#  points(adjustedC, adjustedPressure, ylim=rev(range(unlist(adjusted[["pressure"]]), na.rm=TRUE)), pch=16, col="red")
#  legend("bottomright", col=c("blue","red"), c("Raw", "Adjusted"), pch=c(16, 16))

## ----eval=FALSE---------------------------------------------------------------
#  plot(rawC, adjustedC,
#       xlab=expression("Raw Chlorophyll (mg/m3)"),
#       ylab=expression("Adjusted Chlorophyll (mg/m3)"))
#  abline(0, 1, lty=3)
#  model <- lm(adjustedC ~ rawC)
#  abline(model, col=2)
#  summary(model)

