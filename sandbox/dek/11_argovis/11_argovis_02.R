library(oce)
## https://argovis.colorado.edu/selection/profiles/1/2020
library(rjson)
base <- "https://argovis.colorado.edu/selection/profiles"
month <- 1
year <- 2020
source <- paste0(base, "/", month, "/", year)
file <- paste0(month, "_", year, ".json")
if (!file.exists(file)) download.file(source, file) else cat("Using pre-downloaded file '", file, "'\n", sep="")
a <- fromJSON(file=file)

## Discover file contents by visual inspection.
cat("There are ", length(a), " profiles in month ", month, " of year ", year, "\n")
cat("This is what the first entry holds\n")
cat(str(a[[1]]))

ids <- sapply(a, function(x) x[["_id"]])
lons <- sapply(a, function(x) x[["lon"]])
lats <- sapply(a, function(x) x[["lat"]])
params <- sapply(a, function(x) paste0(x[["station_parameters"]], collapse=","))
dacs <- sapply(a, function(x) paste0(x[["dac"]], collapse=","))
times <- oce::numberAsPOSIXct(sapply(a, function(x) as.POSIXct(gsub("Z$", "", x[["date"]]), "%Y-%m-%dT%H:%M:%S", tz="UTC")))
n <- 20
cat("Some elements of the first", n, "profiles are:\n")
df <- data.frame(ID=ids, longitude=lons, latitude=lats, parameters=params, dac=dacs, time=times)
print(head(df, n))

