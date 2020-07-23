if (!interactive()) png("flowchart.png", unit="in", width=5, height=2.8, pointsize=10, res=120)

colText <- "darkblue"
colCode <- "black"
library(graphics)
textInBox <- function(x, y, text, cex=1, pos=4, center=TRUE, family="Times New Roman", col="black", tweakx=0)
{
    w <- graphics::strwidth(text, family=family)
    h <- graphics::strheight(text, family=family)
    if (center)
        x <- x - w/2
    text(x+tweakx, y, text, cex=cex, pos=pos, family=family, col=col)
    rect(x+tweakx, y-h, x+tweakx+1.1*w, y+h, border=col)
    invisible(list(w=w, h=h))
}
omar <- par("mar")
par(mar=c(0,1,0,0))
plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="", axes=FALSE)
x0 <- 0.25
y0 <- 0.9
dy <- 0.2
wh <- textInBox(x0, y0, "Get an index of profiles from an argo server.", col=colText)
h <- wh$h * 1.2
xarrow <- x0 + 0.01 # not sure why we need to move it
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, "Focus on a subset of profiles.", col=colText)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, "Get profile data files from the server.", col=colText)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, " Read profile data files. ", col=colText)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, " Process the data.  ", col=colText)
x0 <- 0.8
y0 <- 0.9
wh <- textInBox(x0, y0, "getIndex()  ", family="sans", col=colCode)
h <- wh$h * 1.2
xarrow <- x0 + 0.01 # not sure why we need to move it
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1)
y0 <- y0 - dy
textInBox(x0, y0, "subset()  ", family="sans", col=colCode)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1)
y0 <- y0 - dy
textInBox(x0, y0, "getProfiles()  ", family="sans", col=colCode)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1)
y0 <- y0 - dy
textInBox(x0, y0, "readProfiles()  ", family="sans", col=colCode)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1)
y0 <- y0 - dy
textInBox(x0, y0, ". . .   ", family="sans", col=colCode, tweakx=0.005)
par(mar=omar)
if (!interactive()) dev.off()
