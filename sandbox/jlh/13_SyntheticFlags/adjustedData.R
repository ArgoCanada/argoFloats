# Demo for Tanya Maurer
# install.packages('devtools')
# install.packages('oce')
# library(oce)
# devtools::install_github("ArgoCanada/argoFloats", ref='develop')
library(argoFloats)

# 1. Acquire bgc index (this creates an object that holds the index
# plus information on the source, and on the local directory in which
# files will get downloaded in later steps. Note: The files download into
# the present directory. 
bai <- getIndex('synthetic', destdir='.') # slow but faster later (1 week cache)

# 2. Subset to profiles near the Arabian Sea
lonRect <- c(54, 70)
latRect <- c(21, 23)
s <- subset(bai, rectangle=list(longitude=lonRect, latitude=latRect))
ss <- subset(s, ID='5903586')

# 3. Download the relevant netcdf files (or skip downloading, if
# the files have already been stored locally).  Thus, as with
# getIndex(), this is fast after the first time.
profiles <- getProfiles(ss, debug=5)

# 4. Read those netcdf files. Note that we could put a string
# naming files in the next function call, but the above is
# partly designed as a mini-tutorial on using the argoFloats
# package without worrying about URLs, filenames, etc.
argos <- readProfiles(profiles)

# 5. Focus on the first object within the 'argos' object
a <- argos[[1]]

# 6. plot oxygen, raw and adjusted
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
ID <- argos[['ID']][[1]]

# 7. plot contrast between raw and ajusted oxygen
mtext(paste("Float", ID), adj=0)
plot(oxygenAdjusted-oxygenUnadjusted, pressureAdjusted,
     ylim=rev(range(pressureAdjusted, na.rm=TRUE)),
     xlab="Oxygen Diff (umol/kg)", ylab="Pressure (dbar)")
grid()

