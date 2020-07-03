## ----workflow, echo=FALSE, fig.cap="Figure 1: Work flow of the argoFloats package.", fig.height=2.8, fig.width=5, fig.align='center', dev.args=list(pointsize=10), warning=FALSE----

colText <- "black"
colCode <- "black"
library(graphics)
textInBox <- function(x, y, text, cex=1, pos=4, center=TRUE, family="Times New Roman", col="black", tweakx=0)
{
    w <- graphics::strwidth(text)
    h <- graphics::strheight(text)
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
dy <- 0.3
wh <- textInBox(x0, y0, "    What is the quality of the data?", col=colText)
h <- wh$h * 1.2
xarrow <- x0 + 0.01 # not sure why we need to move it
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, "   What tests were performed?", col=colText)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1, col=colText)
y0 <- y0 - dy
textInBox(x0, y0, "   Set low-quality data to NA.", col=colText)
x0 <- 0.8
y0 <- 0.9
wh <- textInBox(x0, y0, "plot(which='QC') ", family="sans", col=colCode)
h <- wh$h * 1.2
xarrow <- x0 + 0.01 # not sure why we need to move it
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1)
y0 <- y0 - dy
textInBox(x0, y0, "showQCTests() ", family="sans", col=colCode)
arrows(xarrow, y0-h, xarrow, y0-dy+h, length=0.1)
y0 <- y0 - dy
textInBox(x0, y0, "applyQC()  ", family="sans", col=colCode)

## ---- warning=FALSE, error=FALSE, message=FALSE-------------------------------
library(argoFloats)
data("index")
subset <- subset(index, ID='1901584')
profiles <- getProfiles(subset)
argos <- readProfiles(profiles)
plot(argos, which='QC', parameter='temperature')

## ---- message=FALSE, error=FALSE, warning=FALSE-------------------------------
library(argoFloats)
data('index')
subset <- subset(index, 1)
profiles <- getProfiles(subset)
argos <- readProfiles(profiles)
argos1 <- argos[[1]]
showQCTests(argos[[1]])

## ---- message=FALSE, error=FALSE, warning=FALSE-------------------------------
library(argoFloats)
# Contrast TS diagrams for raw and flag-handled data
data(index)
i <- subset(index, ID='1901584')
raw <- readProfiles(getProfiles(i))
clean <- applyQC(raw)
par(mfrow=c(1, 2))
plot(raw, which="TS")
plot(clean, which="TS")

## ---- warning=FALSE, error=FALSE, message=FALSE-------------------------------
library(argoFloats)
data("index")
subset <- subset(index, ID='1901584')
profiles <- getProfiles(subset)
raw <- readProfiles(profiles)
adj <- useAdjusted(raw)
par(mfrow=c(1, 2))
oce::plotProfile(oce::as.ctd(raw[[1]]), xtype="temperature")
mtext("Raw data", side=3, line=-1, col=2)
oce::plotProfile(oce::as.ctd(adj[[1]]), xtype="temperature")
mtext("Adjusted data", side=3, line=-1, col=2)

