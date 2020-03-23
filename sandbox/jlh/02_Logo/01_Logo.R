library(oce)
library(argoFloats)
source("01_location.R")

createPng <- TRUE
data(coastlineWorldFine, package="ocedata")
i1 <- getIndex(destdir="~/data/argo")

## The FALSE block is how the topo*.nc file was created.  It is
## in a FALSE block to prevent users from having to download th
## 'dc' package from github.com/dankelley/dc
if (FALSE) {
    library(dc)
    topoFile <- dc.topo(place$longitude-12, place$longitude+12,
                        place$latitude-15, place$latitude+15,
                        resolution=1)
}
topoFile <- "topo_89.33W_65.33W_10.07N_40.07N_1min_gmt.nc"
topo <- read.topo(topoFile)
tlon <- topo[["longitude"]]
tlat <- topo[["latitude"]]
tz <- topo[["z"]]
par(mfrow=c(1,2))

for (radius in seq(100, 1000, 100)) {
    message("processing for radius=", radius)
    i2 <- subset(i1, circle=list(longitude=place$longitude, latitude=place$latitude, radius=radius))
    S <- 1.1
    latitudelim <- place$latitude + S * radius / 111 * c(-1, 1)
    longitudelim <- place$longitude + S * radius / 111 / cos(place$latitude*pi/180) * c(-1, 1)
    if (createPng)
        png(sprintf("02_map_%04dkm.png", radius), width=7, height=7, unit="in", res=120, pointsize=12)
    col1000 <- "green"
    col2000 <- "blue"
    lwd1000 <- 1.8*par("lwd")
    lwd2000 <- 1.8*par("lwd")
    lty1000 <- 1
    lty2000 <- 1
    par(mar=c(2,2,1,1))
    mapPlot(coastlineWorldFine, projection=paste0("+proj=merc +lon_0=", place$longitude),
            latitudelim=latitudelim,
            longitudelim=longitudelim, col="tan")
    mapPoints(i2[["longitude"]], i2[["latitude"]], pch=20, cex=1.4, col=rgb(1, 0, 0, 0.5))

    col <- rev(oceColorsGebco(5, region="water", type="line"))
    mapContour(tlon, tlat, -tz, levels=1000, col=col1000, lwd=lwd1000, lty=lty1000, drawlabels=FALSE)
    mapContour(tlon, tlat, -tz, levels=2000, col=col2000, lwd=lwd2000, lty=lty2000, drawlabels=FALSE)
    legend("topleft", title="Water Depth [m]", bg="white", seg.len=3,
           lty=c(lty1000,lty2000),
           lwd=c(lwd1000, lwd2000),
           col=c(col1000, col2000),
           legend=c("1000", "2000"), cex=0.7,'topleft')
    #mtext(paste0(length(i2[["longitude"]]), " Argo profiles within ", radius, " km of ", place$name))
    if (createPng)
        dev.off()
}


## Creating second panel map
getIndex(
    server = "ftp://usgodae.org/pub/outgoing/argo",
    file = "ar_index_global_prof.txt.gz",
    destdir = "~/data/argo",
    age = 6,
    quiet = FALSE,
    debug = 3
)
if (!exists("ai"))
    ai <- getIndex(file='ar_index_global_prof.txt.gz', destdir="~/data/argo",debug=3)

i2 <- subset(ai, circle=list(longitude=-77.3504, latitude=25.0443, radius=180))
bahamProfiles <- getProfiles(i2, retries=3)
bahamRead <- readProfiles(bahamProfiles) # To read each of the 368 profiles
temperature <- unlist(lapply(bahamRead, function(bahamProfile) handleFlags(bahamProfile)[["temperature"]]))
salinity <- unlist(lapply(bahamRead, function(bahamProfile) handleFlags(bahamProfile)[["salinity"]]))
pressure <- unlist(lapply(bahamRead, function(bahamProfile) handleFlags(bahamProfile)[["pressure"]]))
ctd <- as.ctd(temperature,salinity,pressure)
plot(temperature, pressure, ylim=rev(range(pressure, na.rm=TRUE)), xlab= 'Temperature (C)', ylab=' Pressure (m)')


# Now learning hexSticker to figure out logo
install.packages('tidyverse')
library(tidyverse)
install.packages('magick')
library(magick)
install.packages('png')
library(png)
install.packages('sp')
library(sp)
install.packages('grid')
library(grid)
install.packages('here')
library(here)
install.packages('hexSticker')
library(hexSticker)
install.packages('ggplot2')
library(ggplot2)
tempPlot <- plot(temperature, pressure, ylim=rev(range(pressure, na.rm=TRUE)), xlab= 'Temperature (C)', ylab=' Pressure (m)')
presRev <- rev(pressure)
df <- data.frame(temperature, presRev)

imgurl <- system.file("argoPractice.png", package="argoFloats")
sticker(imgurl, package="argoFloats", p_size=20, s_x=1, s_y=.75, s_width=.6,
        filename="inst/figures/imgfile.png")

sticker(~plot(df),
        package="argoFloats", p_size=5, s_x=.8, s_y=.6, s_width=1.4, s_height=1.2,
        filename="practice.png", p_x=1, p_y=1.85, h_fill='white', p_color='red', h_color='red')
 



img <- readPNG('argoFloatsLogo')
