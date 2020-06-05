library(argoFloats)
if (!exists("bai")) {
    bai <- getIndex('synthetic', age=0)
    lonRect <- c(54, 70)
    latRect <- c(21,23)
    s <- subset(bai, rectangle=list(longitude=lonRect, latitude=latRect))
    profiles <- getProfiles(s)
    argos <- readProfiles(profiles)
}
a <- argos[[1]]
oxygenAdjusted <- a[['oxygenAdjusted']]
pressureAdjusted <- a[['pressureAdjusted']]
oxygenBad <- a[['oxygen']]
pressureBad <- a[['pressure']]
if (!interactive()) png("11_adjusted_data.png", unit="in", width=7, height=3.2, pointsize=11, res=150)
par(mar=c(3,3,1,1))
plot(oxygenAdjusted, pressureAdjusted, ylim=rev(range(pressureAdjusted, na.rm=TRUE)), xlab="Oxygen (umol/kg)", ylab="Pressure (dbar)")
points(oxygenBad, pressureBad, col='red')
legend("bottomright", c("Oxygen Adjusted", "Oxygen Bad"), col=c('black','red'), pch=c(1,1))
if (!interactive()) dev.off()