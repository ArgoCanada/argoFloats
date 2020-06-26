library(oce)
# https://github.com/earthcube2020/ec20_tucker_etal/blob/master/EC2020_argovis_python_api.ipynb
library(rjson)
base <- "https://argovis.colorado.edu/catalog/profiles/"
float <- "2902204"
profile <- "131"
source <- paste0(base, float, "_", profile)
file <- paste0(float, "_", profile, ".json")
if (!exists(file))
    download.file(source, file)
a <- fromJSON(file=file)

## Discover file contents by visual inspection.  Let's hope that the naming
## system is uniform, or writing code for this json format will be trickly.
## A bad sign is that some items are in lower-case, others in upper-case, which
## might suggest that some items were converted from netcdf names, but others
## were not, perhaps suggesting changes in the future, if someone writes code
## to alter the remaining netcdf-style names for uniformity.
message("items in object: ", paste(sort(names(a)), collapse=" "))
stationParameters <- a$station_parameters
stationParametersInNc <- a[[3]]
dataMode <- a$DATA_MODE
platformType <- a$PLATFORM_TYPE
temperature <- unlist(lapply(a$measurements, function(x) x$temp))
salinity <- unlist(lapply(a$measurements, function(x) x$psal))
pressure <- unlist(lapply(a$measurements, function(x) x$pres))

if (!interactive()) png("11_argovis_01.png")
ctd <- as.ctd(salinity, temperature, pressure, longitude=a$lon, latitude=a$lat)
plot(ctd)
if (!interactive()) dev.off()

