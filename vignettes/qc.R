## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment="#>")

## ---- warning=FALSE, error=FALSE, message=FALSE, eval=FALSE-------------------
#  library(argoFloats)
#  data("index")
#  subset <- subset(index, id='1901584')
#  profiles <- getProfiles(subset)
#  argos <- readProfiles(profiles)
#  plot(argos, which='QC', parameter='temperature')

## ---- message=FALSE, error=FALSE, warning=FALSE, eval=FALSE-------------------
#  library(argoFloats)
#  data('index')
#  s <- subset(index, id='1901584')
#  subset <- subset(s, cycle='124')
#  profiles <- getProfiles(subset)
#  argos <- readProfiles(profiles)
#  argos1 <- argos[[1]]
#  showQCTests(argos[[1]])

## ---- message=FALSE, error=FALSE, warning=FALSE, eval=FALSE-------------------
#  library(argoFloats)
#  # Contrast TS diagrams for raw and flag-handled data
#  data(index)
#  i <- subset(index, id='1901584')
#  raw <- readProfiles(getProfiles(i))
#  clean <- applyQC(raw)
#  par(mfrow=c(1, 2))
#  plot(raw, which="TS")
#  plot(clean, which="TS")

## ---- warning=FALSE, error=FALSE, message=FALSE, eval=FALSE-------------------
#  library(argoFloats)
#  raw <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"))
#  adj <- useAdjusted(raw)
#  par(mfrow=c(1,2), mar=c(5,4,1,2))
#  hist(raw[[1]][['oxygen']], xlab='Raw Oxygen', ylab="Frequency", main=NULL)
#  hist(adj[[1]][['oxygen']], xlab='Adjusted Oxygen', ylab="Frequency", main=NULL)
#  summary(unlist(raw[['oxygen']]))
#  summary(unlist(adj[['oxygen']]))
#  summary(unlist(raw[['oxygen']]))
#  summary(unlist(adj[['oxygen']]))

