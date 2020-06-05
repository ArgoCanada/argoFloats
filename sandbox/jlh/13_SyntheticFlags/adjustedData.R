# Demo for Tanya Maurer
# install.packages('devtools')
# install.packages('oce')
# library(oce)
# devtools::install_github("ArgoCanada/argoFloats", ref='develop')
library(argoFloats)
# The following line  may take 1-2 minutes for the first time, and then is
# good for a week
bai <- getIndex('synthetic')
# Subsetting near the Arabian Sea
lonRect <- c(54, 70)
latRect <- c(21, 23)
s <- subset(bai, rectangle=list(longitude=lonRect, latitude=latRect))
profiles <- getProfiles(s)
argos <- readProfiles(profiles)
a <- argos[[1]]
ID0 <- argos[['ID']][[1]]
oxygenAdjusted <- a[['oxygenAdjusted']]
pressureAdjusted <- a[['pressureAdjusted']]
oxygenUnadjusted <- a[['oxygen']]
pressureUnadjusted <- a[['pressure']]
par(mar=c(3,3,2,1), mfrow=c(1,2), mgp=c(2,0.7,0))
plot(oxygenAdjusted, pressureAdjusted,
     ylim=rev(range(pressureAdjusted, na.rm=TRUE)),
     xlab="Oxygen (umol/kg)", ylab="Pressure (dbar)")
grid()
points(oxygenUnadjusted, pressureUnadjusted, col='red')
legend("bottomright",
       c("Oxygen Adjusted", "Oxygen Unadjusted"),
       col=c('black','red'), cex=0.75, pch=c(1,1), bg='white')
mtext(paste("Float", ID0), adj=0)
plot(oxygenAdjusted-oxygenUnadjusted, pressureAdjusted,
     ylim=rev(range(pressureAdjusted, na.rm=TRUE)),
     xlab="Oxygen Diff (umol/kg)", ylab="Pressure (dbar)")
grid()
