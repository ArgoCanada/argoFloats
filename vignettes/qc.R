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
#  library(argoFloats)
#  data('index')
#  index1 <- subset(index, id='1901584')
#  index2 <- subset(index1, cycle='124')
#  profiles2 <- getProfiles(index2)
#  argos2 <- readProfiles(profiles2)
#  showQCTests(argos2[[1]])

## ---- message=FALSE, error=FALSE, warning=FALSE, eval=FALSE-------------------
#  library(argoFloats)
#  # Contrast TS diagrams for raw and flag-handled data
#  data(index)
#  index1 <- subset(index, id='1901584')
#  argos <- readProfiles(getProfiles(index1))
#  clean <- applyQC(argos)
#  par(mfrow=c(1, 2))
#  plot(argos, which="TS")
#  plot(clean, which="TS")

## ---- eval=FALSE--------------------------------------------------------------
#  library(argoFloats)
#  data(index)
#  i <- subset(index, id='1901584')
#  raw <- readProfiles(getProfiles(i))
#  clean <- applyQC(raw)
#  A <- raw[[1]]
#  D <- data.frame(T = A[['temperature']], TF=A[['temperatureFlag']], S=A[['salinity']], SF=A[['salinityFlag']], P=A[['pressure']], PF=A[['pressureFlag']])
#  head(D)

## ---- warning=FALSE, error=FALSE, message=FALSE, eval=FALSE-------------------
#  library(argoFloats)
#  raw <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"))
#  adj <- useAdjusted(raw)
#  par(mfrow=c(1,2), mar=c(5,4,1,2))
#  hist(raw[[1]][['oxygen']], xlab='Raw Oxygen', ylab="Frequency", main=NULL)
#  hist(adj[[1]][['oxygen']], xlab='Adjusted Oxygen', ylab="Frequency", main=NULL)
#  summary(unlist(raw[['oxygen']]))
#  summary(unlist(adj[['oxygen']]))

