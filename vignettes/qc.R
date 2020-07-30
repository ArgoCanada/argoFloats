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
#  par(mfrow=c(1, 2))
#  oce::plotProfile(oce::as.ctd(raw[[1]]), xtype="oxygen")
#  mtext("Raw data", side=3, line=-1, col=2)
#  oce::plotProfile(oce::as.ctd(adj[[1]]), xtype="oxygen")
#  mtext("Adjusted data", side=3, line=-1, col=2)
#  summary(unlist(raw[['oxygen']]))
#  summary(unlist(adj[['oxygen']]))

