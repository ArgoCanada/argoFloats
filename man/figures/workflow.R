colText <- "darkblue"
colCode <- "black"
PNG <- TRUE
library(graphics)
textInBox <- function(x, y, text, cex=1, pos=4, center=TRUE, family="Times New Roman", col="black")
{
    message(text)
    message("  x=", x, " y=", y)
    w <- graphics::strwidth(text)
    h <- graphics::strheight(text)
    message("  w=", w, ", h=", h)
    if (center)
        x <- x - w/2
    message("  x=", x, " y=", y)
    text(x, y, text, cex=cex, pos=pos, family=family, col=col)
    rect(x, y-h, x+w, y+h, border=col)
    invisible(list(w=w, h=h))
}
if (PNG) png("workflow.png", width=3.5, height=2, unit="in", res=130, pointsize=8)
par(mar=rep(0,4))
plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="", axes=FALSE)
x0 <- 0.3
y0 <- 0.9
dy <- 0.2
wh <- textInBox(x0, y0, "Get index of profiles from server", col=colText)
h <- wh$h * 1.2
length <- 0.07
arrows(x0, y0-h, x0, y0-dy+h, length=length, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, "Focus on a subset of profiles", col=colText)
arrows(x0, y0-h, x0, y0-dy+h, length=length, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, "Get profile data files from server", col=colText)
arrows(x0, y0-h, x0, y0-dy+h, length=length, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, "Read profile data files", col=colText)
arrows(x0, y0-h, x0, y0-dy+h, length=length, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, "Process the data", col=colText)

x0 <- 0.8
y0 <- 0.9
dy <- 0.2
wh <- textInBox(x0, y0, "getIndex()    ", family="sans", col=colCode)
h <- wh$h * 1.2
arrows(x0, y0-h, x0, y0-dy+h, length=length)
y0 <- y0 - dy
textInBox(x0, y0, "subset()    ", family="sans", col=colCode)
arrows(x0, y0-h, x0, y0-dy+h, length=length)
y0 <- y0 - dy
textInBox(x0, y0, "getProfiles()    ", family="sans", col=colCode)
arrows(x0, y0-h, x0, y0-dy+h, length=length)
y0 <- y0 - dy
textInBox(x0, y0, "readProfiles()    ", family="sans", col=colCode)
arrows(x0, y0-h, x0, y0-dy+h, length=length)
y0 <- y0 - dy
textInBox(x0, y0, " ...      ", family="sans", col=colCode)

if (PNG) dev.off()

