drawAxes <- FALSE
drawR <- FALSE
library(oce)
library(argoFloats)
library(hexSticker)
library(cmocean) # for depth colours

data(coastlineWorldFine, package="ocedata")
topo <- read.topo("topo_81W_72W_22N_31N_1min_gmt.nc")

bright <- "#DBA748" # https://www.rapidtables.com/web/color/color-scheme.html
dark <- hsv(177/255, 0.67, 0.57)
water <- rgb(0, 0, 1, alpha=0.15)
floats <- "#c02a2a33"                  # red, but not glaring
floats <- "#FF0000"
floats <- "#FF000055"                  # red, but not glaring
lwd <- 0.7                             # for axes
border <- "#91653c"
fg <- dark
bg <- bright


a <- function()
{
    oldpar <- par(no.readonly=TRUE)
    par(mar=c(0,0,0,0), cex=0.6, mgp=c(1.8, 0.7, 0), font=2, lwd=0.8)
    data("index")
    lon <- index[["longitude"]]
    lat <- index[["latitude"]]
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
         cex=0.3, axes=FALSE)
    n <- 256
    imagep(topo[["longitude"]], topo[["latitude"]], topo[["z"]],
           ##col=rev(cmocean("deep")(n)),
           col=oce::oceColorsGebco(n),
           breaks=seq(0, -5000, length.out=1+n),
           zlim=c(-5000, 0),
           add=TRUE)
    if (drawAxes) {
        axis(1, at=seq(-79, -75, 1), label=FALSE, lwd=lwd)
        axis(1, at=seq(-79, -75, 2), label=paste0(seq(79,75,-2),"W"), lwd=lwd)
        axis(2, at=seq(25, 28, 1), label=FALSE, lwd=lwd)
        axis(2, at=seq(25, 28, 2), label=paste0(seq(25, 28, 2),"N"), lwd=lwd)
    }
    points(lon, lat, pch=21, col="white", bg="red", cex=0.4, lwd=0.2)
    box(lwd=lwd)
    polygon(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]],
            lwd=0.5, col="tan", xpd=FALSE)
    par(xpd=NA)
    if (drawR)
        mtext("R", col=fg, cex=1.8, font=2, side=3, line=3)
    par(oldpar)
}

sticker(~a(), package="argoFloats",
        s_x=if (drawAxes) 0.92 else 0.875,
        s_y=if (drawAxes) 0.71 else 0.61,
        s_width=if (drawAxes) 0.99 else 1.12,
        s_height=if (drawAxes) 0.99 else 1.12,
        h_fill=bg,
        h_color=fg,
        p_color=fg,
        filename="argoFloats_logo_06.png")
