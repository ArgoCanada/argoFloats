library(oce)
library(png)
library(hexSticker)
library(ocedata)

topo <- read.topo("topo_87.8W_78.3W_7.4N_10.61N_1min.nc")
data(coastlineWorldFine, package="ocedata")


# Step 1: create png

createBackground <- function()
{
    png("background2.png", width=4, height=4, unit="in", pointsize=6, res=144)
    library(argoFloats)
    ai <- getIndex(age=40)
    s <- subset(ai, circle=list(longitude=-83, latitude=9, radius=150))
    
    lon <- s[["longitude"]]
    lat <- s[["latitude"]]
    asp <- 1 / cos(pi/180*mean(range(lat)))
    D <- 0.33                          # space around float cloud
    xlim <- range(lon) + D*c(-1, 1)
    ylim <- range(lat) + D*c(-1, 1)
    ## Turn off axis labels, so we can save space by writing W and N
    ## by th numbers, and also get sub-ticks.
    plot(lon, lat, asp=asp,
         xlim=xlim, ylim=ylim,
         xlab="",#expression("Longitude ["*degree*"W]"),
         ylab="",#expression("Latitude ["*degree*"N]"),
         type="n",
         cex=0.3, axes=FALSE, pch=21)
    n <- 256
    imagep(topo[["longitude"]], topo[["latitude"]], topo[["z"]],
           col=oce::oceColorsGebco(n),
           breaks=seq(-3500,1000, length.out=1+n),
           zlim=c(-5000, 0),
           add=TRUE)
    polygon(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]],
            lwd=0.5, col="tan", xpd=FALSE)
    pax <- c(-83.31811, -81.46290, -81.46290, -82.33111, -83.10792, -83.40950)
    pay <- c(10.365979, 10.365979,  8.876569,  9.048076,  9.698001, 10.384033)
    # Now select your points on the map in the Atlantic
    a <- subset(s, polygon=list(longitude=pax, latitude=pay))
    points(a[['longitude']], a[['latitude']], pch=21, col='black', bg='red', lwd=1)
    ppx <- c(-84.50618, -83.41864, -81.82846, -81.60913, -83.19931, -84.60671, -84.61585)
    ppy <- c(9.643841, 8.939756, 7.919735, 7.621853, 7.630880, 7.657960, 9.445253)
    # Now select your points on the map in the Pacific
    p <- subset(s, polygon=list(longitude=ppx, latitude=ppy))
    points(p[['longitude']], p[['latitude']],pch=21, col='black', bg='yellowgreen', lwd=1)
    # Adding spice graph
    par(new=TRUE, mar=c(7,6,12,5))
    dev.off()
}

# HINT: play with transparency, in range 0 to 1
createBackground()

dark <- "black"
bright <- "black" # https://www.rapidtables.com/web/color/color-scheme.html
dark <- hsv(177/255, 0.67, 0.57)
water <- rgb(0, 0, 1, alpha=0.15)      # red, but not glaring
lwd <- 0.7                             # for axes
border <- "dimgray"
fg <- dark
bg <- bright

sticker(subplot="background2.png",
        package="argoFloats",
        h_fill=bg,
        h_color=border,
        p_x=1.0,
        p_y=1.1,
        p_size=17,
        p_family = "sans",
        p_fontface="bold",
        p_color="dimgray",
        s_x=1.0,
        s_y=1.05,
        s_width=1.5,
        s_height=1.5,
        white_around_sticker = TRUE,
        filename="argoTag_hex_sticker2.png")
