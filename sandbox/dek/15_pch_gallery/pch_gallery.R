n <- 25
scale <- 5
png("pch_gallery.png", width=300, height=300, pointsize=18)
par(mar=rep(0, 4))
plot(c(0, scale), c(0, scale), type="n", xlab="", ylab="", axes=FALSE)
dx <- scale / 5
dy <- scale / 6
x0 <- dx/2
y0 <- scale - dy/2
x <- x0
y <- y0
cex <- 1.4
lwd <- 1.4
for (i in seq(0, n)) {
    points(x, y, pch=i, cex=cex, lwd=lwd)
    text(x, y, i, pos=2)
    x <- x + dx
    if (0 == (1+i)%%5) {
        x <- x0
        y <- y - dy
    }
}
dev.off()

