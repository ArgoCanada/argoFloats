library(oce)
library(argoFloats)
library(hexSticker)

data(coastlineWorldFine, package="ocedata")
topo <- read.topo("topo_81W_72W_22N_31N_1min_gmt.nc")

dark <- "#4a4326"
bright <- "#DBA748" # https://www.rapidtables.com/web/color/color-scheme.html
dark <- hsv(177/255, 0.67, 0.57)
water <- rgb(0, 0, 1, alpha=0.15)
floats <- "#c02a2a33"                  # red, but not glaring
floats <- "#FF000055"                  # red, but not glaring
lwd <- 0.7                             # for axes
border <- "#91653c"
fg <- dark
bg <- bright


a <- function()
{
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
    z <- topo[["z"]]
    #z[z > 0] <- NA
    cm <- colormap(z, name="gmt_gebco")
    imagep(topo[["longitude"]], topo[["latitude"]], z, colormap=cm, add=TRUE, missingColor='red')
    axis(1, at=seq(-79, -75, 1), label=FALSE, lwd=lwd)
    axis(1, at=seq(-79, -75, 2), label=paste0(seq(79,75,-2),"W"), lwd=lwd)
    axis(2, at=seq(25, 28, 1), label=FALSE, lwd=lwd)
    axis(2, at=seq(25, 28, 2), label=paste0(seq(25, 28, 2),"N"), lwd=lwd)
    usr <- par("usr")
    points(lon, lat, bg=floats, col=floats, pch=21, cex=0.2)
    box(lwd=lwd)
    polygon(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]],
            lwd=0.5, border=gray(0.2), col=border, xpd=FALSE)
    par(xpd=NA)
    mtext("R", col=fg, cex=1.8, font=2, side=3, line=3)
}

sticker(~a(), package="argoFloats",
        s_x=0.92,
        s_y=0.71,
        s_width=0.99,
        s_height=0.99,
        h_fill=bg,
        h_color=border,
        p_color=fg,
        filename="argoFloats_logo_02.png")

