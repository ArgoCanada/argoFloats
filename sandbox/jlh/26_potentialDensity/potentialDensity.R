rm(list=ls())
library(argoFloats)
library(oce)
data("index")
index1 <- subset(index, id='1901584')
index2 <- subset(index1, cycle='124') 
profiles2 <- getProfiles(index2)
argos2 <- readProfiles(profiles2)
a <- argos2[[1]]
#potDen <- function(x, p_ref=0) {
   # S <- x[['salinity']]
    #T <- x[['temperature']]
    #p <- x[['pressure']]
    #swRho(S, swTheta(S, T, p, p_ref), p_ref)
#}
#potDen(a)

densityTest <- function(x, criterion = 0.03) {
    mids <- function(x) head(x, -1) + diff(x)/2
    S <- x[['salinity']]
    T <- x[['temperature']]
    p <- x[['pressure']]
    pmids <- mids(p)
    n <- length(S)
    # Check from top to bottom
    rhoDiffTopDown <- rep(NA, n)
    for (i in seq_len(n-1)) {
        rho1 <- swRho(S[i], swTheta(S[i], T[i], p[i], pmids[i]), pmids[i])
        rho2 <- swRho(S[i+1], swTheta(S[i+1], T[i+1], p[i+1], pmids[i]), pmids[i])
        rhoDiffTopDown[i] <- rho2 - rho1
    }
    okTopDown <- rhoDiffTopDown > (-criterion)
    return(list(rhoDiffTopDown=rhoDiffTopDown, okTopDown=okTopDown))
}
dt <- densityTest(a)
D <- data.frame(SF= a[['salinityFlag']], TF=a[['temperatureFlag']], rhoDiff=dt$rhoDiff, ok=dt$ok)
print(D)