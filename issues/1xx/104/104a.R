library(argoFloats)
if (!exists("profiles")) { # cache for speed of interactive retesting
    bai <- getIndex(filename='merge')
    s <- subset(bai, circle=list(longitude=-83, latitude=9, radius=800))
    pax <- c(-92.56199, -87.32731, -83.66646, -79.35555, -88.28529, -92.76727, -92.76727, -92.73306)
    pay <- c(15.204447, 12.422940,  9.071727,  5.653489,  5.720514,  5.653489, 10.546261, 15.439032)
    a <- subset(s, polygon=list(longitude=pax, latitude=pay))
    subo <- subset(a, parameter='DOXY')
    profiles <- getProfiles(subo)
}
library(ncdf4)
## source("~/git/argoFloats/R/read.R")
## source("~/git/oce/R/processingLog.R")
## source("~/git/oce/R/AllClass.R")
argos <- readProfiles(profiles, handleFlags=FALSE)
##pressure <- unlist(argos[["pressure"]])
##oxygen <- unlist(argos[["oxygen"]])
##summary(oxygen)

