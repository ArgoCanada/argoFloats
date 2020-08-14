# QC Plot
library(argoFloats)
data("index")
index1 <- subset(index, id='1901584')
profiles <- getProfiles(index1)
argos <- readProfiles(profiles)
clean <- applyQC(argos)
plot(clean, which="TS")

salinity <- unlist(x[["salinity", debug=debug]])
temperature <- unlist(x[["temperature", debug=debug]])
pressure <- unlist(x[["pressure", debug=debug]])
## Use byLevel to repeat the latitude and longitude values across
## the depths in each profile, so that the resultant vector
## will match the salinity, temperature and pressure vectors.
latitude <- unlist(x[["latitude", "byLevel", debug=debug]])
longitude <- unlist(x[["longitude", "byLevel", debug=debug]])
ctd <- oce::as.ctd(salinity=salinity,
                   temperature=temperature,
                   pressure=pressure,
                   latitude=latitude,
                   longitude=longitude)