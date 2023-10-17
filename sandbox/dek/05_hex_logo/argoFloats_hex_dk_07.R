useCache <- TRUE # set TRUE to speed runs whilst adjusting aeshetics
library(oce)
library(argoFloats)
library(hexSticker)
library(ocedata)

# Step 1: get data, possibly cached in floats.rda file
data(coastlineWorldFine, package="ocedata")
topoFile <- download.topo(-87.8, -78.3, 7.4, 10.61, res=1)
topo <- read.topo(topoFile)
if (useCache && file.exists("floats.rda")) {
    message("using float positions stored in floats.rda")
    load("floats.rda")
} else {
    ai <- getIndex(age=40)
    s <- subset(ai, circle=list(longitude=-83, latitude=9, radius=150))
    atlanticPolygon <- list(
        longitude=c(-83.31811, -81.46290, -81.46290, -82.33111, -83.10792, -83.40950),
        latitude=c(10.365979, 10.365979,  8.876569,  9.048076,  9.698001, 10.384033))
    atlantic <- subset(s, polygon=atlanticPolygon)
    pacificPolygon <- list(
        longitude=c(-84.50618, -83.41864, -81.82846, -81.60913, -83.19931, -84.60671, -84.61585),
        latitude=c(9.643841, 8.939756, 7.919735, 7.621853, 7.630880, 7.657960, 9.445253))
    pacific <- subset(s, polygon=pacificPolygon)
    message("saving float positions in floats.rda for later use")
    save(atlantic, pacific, file="floats.rda")
}

# Step 2: create png
createBackground <- function()
{
    png("background_07.png", width=4, height=4, unit="in", pointsize=6, res=144)
    lon <- c(atlantic[["longitude"]], pacific[["longitude"]])
    lat <- c(atlantic[["latitude"]], pacific[["latitude"]])
    D <- 0.33                          # space around float cloud
    ylim <- range(lat) + D*c(-1, 1)
    asp <- 1 / cos(pi/180*mean(ylim))
    xlim <- range(lon) + D*c(-1, 1)*asp
    # Plot all points (just to get scale; they are overplotted later)
    plot(lon, lat, asp=asp, xlim=xlim, ylim=ylim,
        xlab="", ylab="", type="n", axes=FALSE)
    n <- 256
    # Topography
    imagep(topo[["longitude"]], topo[["latitude"]], topo[["z"]],
        col=oce::oceColorsGebco(n),
        breaks=seq(-3000,400, length.out=1+n),
        zlim=c(-3000, 0),
        add=TRUE)
    # Land
    polygon(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]],
        lwd=1, col="#F3e6bf", border="#9c7250")
    # Float positions
    points(atlantic[['longitude']], atlantic[['latitude']],
        pch=21, col='black', bg=2, lwd=1, cex=2)
    points(pacific[['longitude']], pacific[['latitude']],
        pch=21, col='black', bg=3, lwd=1, cex=2)
    dev.off()
}

# Step 3: create sticker
createBackground()
bright <- "black" # https://www.rapidtables.com/web/color/color-scheme.html
dark <- hsv(177/255, 0.67, 0.57)
lwd <- 0.7                             # for axes
border <- gray(0.5)
fg <- dark
bg <- bright

sticker(subplot="background_07.png",
        package="argoFloats",
        h_fill=bg,
        h_color=border,
        p_x=1.0,
        p_y=1.05,
        p_size=16,
        p_family = "sans",
        p_fontface="bold",
        p_color=gray(0.3),
        s_x=1.0,
        s_y=1.05,
        s_width=1.5,
        s_height=1.5,
        white_around_sticker = TRUE,
        filename="argoFloats_hex_dk_07.png")
