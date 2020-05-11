library(oce)
library(argoFloats)
library(cmocean) # for depth colours
data(index)
data(coastlineWorldFine, package="ocedata")
if (!exists("topo"))
    topo <- read.topo("topo_81W_72W_22N_31N_1min_gmt.nc")
png("test_bathy_colorscheme.png")
## First plot, to get scales
plot(index)
usr <- par("usr")
#palette <- colorRampPalette(c("#0000FF", "#000088", "#AAAAFF"))
n <- 255
imagep(topo[["longitude"]], topo[["latitude"]], topo[["z"]],
       col=rev(cmocean("deep")(n)),
       breaks=seq(0, -6000, length.out=1+n),
       zlim=c(-5000, 0),
       add=TRUE)
polygon(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]], col="tan")
points(index[["longitude"]], index[["latitude"]], col="#FF0000", cex=1, pch=20)
dev.off()

