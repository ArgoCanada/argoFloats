library(oce)
library(argoFloats)
library(hexSticker)
data(coastlineWorldFine, package="ocedata")


## Two colours picked with "gimp" application from the 2500 year-old
## argonaut vase shown in https://en.wikipedia.org/wiki/Argonauts
bright <- "#d98853"
bright <- "#ffcc99"
dark <- "#4a4326"
bright <- "#DBA748" # https://www.rapidtables.com/web/color/color-scheme.html
dark <- hsv(177/255, 0.67, 0.57)
water <- rgb(0, 0, 1, alpha=0.15)
floats <- "#e71d1d"
## Border is avg between two, developed with e.g.
## > as.raw((0xd9+0x4a)/2)
## [1] 91  
border <- "#91653c"
fg <- dark
bg <- bright



a <- function()
{
    #par(fg=fg, bg=bg)
    par(mar=rep(0,4))
    ## plot(0:1, 0:1, xlab="", ylab="", type="n", axes=FALSE)
    ## x <- 0.5
    ## y <- 0.8
    ## dy <- 0.2
    ## text(x, y, "An R package\ndood\nboy", font=2)
    ## y <- y - dy
    data("index")
    lon <- index[["longitude"]]
    lat <- index[["latitude"]]
    asp <- 1 / cos(pi/180*mean(range(lat)))
    plot(lon, lat, asp=asp, xlab="", ylab="", type="n", axes=FALSE)
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col="white")
    rect(usr[1], usr[3], usr[2], usr[4], col=water, border=bg)
    points(lon, lat, col=floats, pch=20, cex=0.2)
    box(lwd=0.5)
    polygon(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]],
            lwd=0.5, border=gray(0.2), col=border, xpd=FALSE)
    ## plot(index)
}

sticker(~a(), package="argoFloats",
        s_y=0.55,
        s_width=1.25,
        s_height=1.25,
        h_fill=bg,
        h_color=border,
        p_color=fg,                    # tried 'black' but it is too eye-catching
        filename="argoFloats_logo_01.png")

