library(ncdf4)
library(oce)
data(coastlineWorld)

## ftp://data.argo.org.cn/pub/ARGO/BOA_Argo/NetCDF/BOA_Argo_2020_05.nc
## ftp://data.argo.org.cn/pub/ARGO/BOA_Argo/NetCDF/BOA_Argo_2020_04.nc
## ftp://data.argo.org.cn/pub/ARGO/BOA_Argo/NetCDF/BOA_Argo_2020_05.nc
year <- 2019
month <- 12
file <- sprintf("BOA_Argo_%4d_%02d.nc", year, month)
if (!file.exists(file)) {
    url <- "ftp://data.argo.org.cn/pub/ARGO/BOA_Argo/NetCDF"
    source <- paste0(url, "/", file)
    download.file(source, file)
}

n <- nc_open(file)
longitude <- ncvar_get(n, "lon")
latitude <- ncvar_get(n, "lat")
salinity <- ncvar_get(n, "salt")
temperature <- ncvar_get(n, "temp")
pressure <- ncvar_get(n, "pres")

epsilon <- 0.025
for (p in c(0, 100, 250, 500, 1000, 1500, 2000)) {
    ip <- which.min(abs(p - pressure))
    ## Temperature
    if (!interactive())
        png(sprintf("10_gridded_01_T_%04ddbar.png", pressure[ip]),
            width=7, height=3.5, unit="in", res=200)
    Tlim <- quantile(temperature[,,ip], c(epsilon, 1-epsilon), na.rm=TRUE)
    imagep(longitude, latitude, temperature[,,ip], asp=1, zlim=Tlim)
    mtext(paste("Temperature at", pressure[ip], "dbar"))
    polygon(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]], col="tan")
    polygon(coastlineWorld[["longitude"]]+360, coastlineWorld[["latitude"]], col="tan")
    if (!interactive())
        dev.off()
    ## Salinity
    if (!interactive())
        png(sprintf("10_gridded_01_S_%04ddbar.png", pressure[ip]),
            width=7, height=3.5, unit="in", res=200)
    Slim <- quantile(salinity[,,ip], c(epsilon, 1-epsilon), na.rm=TRUE)
    imagep(longitude, latitude, salinity[,,ip], asp=1, zlim=Slim)
    mtext(paste("Salinity at", pressure[ip], "dbar"))
    polygon(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]], col="tan")
    polygon(coastlineWorld[["longitude"]]+360, coastlineWorld[["latitude"]], col="tan")
    if (!interactive())
        dev.off()
}


