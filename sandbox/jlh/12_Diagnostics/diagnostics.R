library(argoFloats)
library(oce)
qf <- function(x) {
    #message(paste0(item, 'Flag'))
    #print(x[[paste0(item, 'Flag')]])
    res <- 100 * (1 - sum(4 == x[[paste0(item, 'Flag')]]) / length(x[[paste0(item, 'Flag')]]))
    #print(summary(x[[paste0(item, 'Flag')]]))
    res
}
#meanf <- function(x) 
#mean(x[[item]])
#minf <- function(x)
#min(x[[item]])
#maxf <- function(x)
#max(x[[item]])
if (!exists("bai")) {
    bai <- getIndex(file='bgc')
    F5901462 <- subset(bai, ID='5901462') # Follow 5901462 float
    profiles <- getProfiles(F5901462)
    argos <- readProfiles(profiles, handleFlags=FALSE) 
}
time <- oce::numberAsPOSIXct(unlist(lapply(argos[['profile']], function(x) x[['time']])))
items <- c('oxygen', 'salinity', 'temperature')
for (item in items) {
    q <- unlist(lapply(argos[['profile']], qf))
    #mean <- unlist(lapply(argos[['profile']], meanf))
    #min <- unlist(lapply(argos[['profile']], minf))
    #max <- unlist(lapply(argos[['profile']], maxf))
    par(mfrow=c(2,1), mar=c(2.5,2.5,1,1))
    if (any(is.finite(q))) {
        oce.plot.ts(time,q, ylab=paste(item, "% Good"), drawTimeRange = FALSE)
        abline(h=50, col='red', lty='dashed')
        oce.plot.ts(time, mean, ylab=paste(item, "Mean"), type='l', col='grey', drawTimeRange = FALSE)
        points(time, mean, col=ifelse(q < 50, 'red', 'black'), pch=20, cex=0.75)
    } else {
        plot(0:1, 0:1, xlab="", ylab='', type="n", axes=FALSE)
        box()
        text(0, 0.5, paste(' No', item, 'flags available'), pos=4)
        plot(0:1, 0:1, xlab="", ylab='', type="n", axes=FALSE)
        box()
        text(0, 0.5, paste(' No', item, 'flags available'), pos=4)
    }
    
    #oce.plot.ts(time,min, ylab=paste(item, "Minimum"))
    #oce.plot.ts(time,max, ylab=paste(item, "Maximum"))
}

