library(ncdf4)
library(oce)
library(curl)

flagIsAcceptable <- function(f1, f2=NULL) # accept if flagged with 1=passed, 2=probably good, 5=changed
{
    if (is.null(f2))
        f2 <- rep(2, length(f1))
    (f1 %in% c(1, 2, 5)) & (f2 %in% c(1, 2, 5))
}

flagCol <- function(f1, f2=NULL) # black if flag is acceptable, red otherwise
{
    if (is.null(f2))
        f2 <- rep(2, length(f1))
    ifelse(flagIsAcceptable(f1) & flagIsAcceptable(f2), "black", "red")
}

# Download file locally, independently of argoFloats
url <- "https://data-argo.ifremer.fr/dac/aoml/5904013/profiles/D5904013_153.nc"
file <- gsub(".*/", "", url)
if (!file.exists(file))
    curl_download(url, file)

# Show mode, checking read.oce() vs direct ncdf4 lookup
n <- nc_open(file)
ncvar_get(n, "DATA_MODE")
d <- read.oce(file)
d[["dataMode"]]

# Examine the data
p <- d[["pressure"]]
pf <- d[["pressureFlag"]]
padj <- d[["pressureAdjusted"]]
padjf <- d[["pressureAdjustedFlag"]]

S <- d[["salinity"]]
Sf <- d[["salinityFlag"]]
Sadj <- d[["salinityAdjusted"]]
Sadjf <- d[["salinityAdjustedFlag"]]

T <- d[["temperature"]]
Tf <- d[["temperatureFlag"]]
Tadj <- d[["temperatureAdjusted"]]
Tadjf <- d[["temperatureAdjustedFlag"]]

npcol <- ncol(p)
if (npcol != ncol(S))
    stop("p and S have differing numbers of columns")
if (npcol != ncol(T))
    stop("p and T have differing numbers of columns")
pf <- data.frame(p1=p[,1], p2=p[,2], p3=p[,3], padj1=padj[,1], padj2=padj[,2], padj3=padj[,3])
head(pf)
tail(pf)

# Unadjusted data (red if flag is not acceptable)
par(mfrow=c(4, npcol), mar=c(3,3,1,1), mgp=c(2,0.7,0))
ctd1 <- as.ctd(S[,1], T[,1], p[,1])
ctd2 <- as.ctd(S[,2], T[,2], p[,2])
ctd3 <- as.ctd(S[,3], T[,3], p[,3])
plotProfile(ctd1, xtype="S", col=flagCol(Sf[,1]), type="p", pch=20)
plotProfile(ctd2, xtype="S", col=flagCol(Sf[,2]), type="p", pch=20)
plotProfile(ctd3, xtype="S", col=flagCol(Sf[,3]), type="p", pch=20)
plotProfile(ctd1, xtype="T", col=flagCol(Tf[,1]), type="p", pch=20)
plotProfile(ctd2, xtype="T", col=flagCol(Tf[,2]), type="p", pch=20)
plotProfile(ctd3, xtype="T", col=flagCol(Tf[,3]), type="p", pch=20)
plotProfile(ctd1, xtype="sigmaTheta", col=flagCol(Tf[,1]), type="p", pch=20)
plotProfile(ctd2, xtype="sigmaTheta", col=flagCol(Tf[,2]), type="p", pch=20)
plotProfile(ctd3, xtype="sigmaTheta", col=flagCol(Tf[,3]), type="p", pch=20)
plotTS(ctd1, col=flagCol(Sf[,1], Tf[,1]))
plotTS(ctd2, col=flagCol(Sf[,2], Tf[,2]))
plotTS(ctd3, col=flagCol(Sf[,3], Tf[,3]))

# Unadjusted data (skip if flag is not acceptable)
par(mfrow=c(4, npcol), mar=c(3,3,1,1), mgp=c(2,0.7,0))
accept <- flagIsAcceptable(Sf[,1], Tf[,1]) # only accept if both S and T are okay (I think p is likely ok)
ctd1 <- as.ctd(S[,1][accept], T[,1][accept], p[,1][accept])
accept <- flagIsAcceptable(Sf[,2], Tf[,2])
ctd2 <- as.ctd(S[,2][accept], T[,2][accept], p[,2][accept])
accept <- flagIsAcceptable(Sf[,3], Tf[,3])
ctd3 <- as.ctd(S[,3][accept], T[,3][accept], p[,3][accept])
plotProfile(ctd1, xtype="S", type="p", pch=20)
plotProfile(ctd2, xtype="S", type="p", pch=20)
plotProfile(ctd3, xtype="S", type="p", pch=20)
plotProfile(ctd1, xtype="T", type="p", pch=20)
plotProfile(ctd2, xtype="T", type="p", pch=20)
plotProfile(ctd3, xtype="T", type="p", pch=20)
plotProfile(ctd1, xtype="sigmaTheta", type="p", pch=20)
plotProfile(ctd2, xtype="sigmaTheta", type="p", pch=20)
plotProfile(ctd3, xtype="sigmaTheta", type="p", pch=20)
plotTS(ctd1)
plotTS(ctd2)
plotTS(ctd3)

# Adjusted data (red if flag is not good, as defined above)
par(mfrow=c(4, npcol), mar=c(3,3,1,1), mgp=c(2,0.7,0))
ctd1 <- as.ctd(Sadj[,1], Tadj[,1], padj[,1])
ctd2 <- as.ctd(Sadj[,2], Tadj[,2], padj[,2])
ctd3 <- as.ctd(Sadj[,3], Tadj[,3], padj[,3])
plotProfile(ctd1, xtype="S", col=flagCol(Sadjf[,1]), type="p", pch=20)
plotProfile(ctd2, xtype="S", col=flagCol(Sadjf[,2]), type="p", pch=20)
plotProfile(ctd3, xtype="S", col=flagCol(Sadjf[,3]), type="p", pch=20)
plotProfile(ctd1, xtype="T", col=flagCol(Tadjf[,1]), type="p", pch=20)
plotProfile(ctd2, xtype="T", col=flagCol(Tadjf[,2]), type="p", pch=20)
plotProfile(ctd3, xtype="T", col=flagCol(Tadjf[,3]), type="p", pch=20)
plotProfile(ctd1, xtype="sigmaTheta", col=flagCol(Sadjf[,1], Tadjf[,1]), type="p", pch=20)
plotProfile(ctd2, xtype="sigmaTheta", col=flagCol(Sadjf[,2], Tadjf[,2]), type="p", pch=20)
plotProfile(ctd3, xtype="sigmaTheta", col=flagCol(Sadjf[,3], Tadjf[,3]), type="p", pch=20)
plotTS(ctd1, col=flagCol(Sf[,1], Tf[,1]))
plotTS(ctd2, col=flagCol(Sf[,2], Tf[,2]))
plotTS(ctd3, col=flagCol(Sf[,3], Tf[,3]))


# Adjusted data (skip if flag is not acceptable (as defined above)
par(mfrow=c(4, npcol), mar=c(3,3,1,1), mgp=c(2,0.7,0))
accept <- flagIsAcceptable(Sadjf[,1], Tadjf[,1])
ctd1 <- as.ctd(Sadj[,1][accept], Tadj[,1][accept], padj[,1][accept])
accept <- flagIsAcceptable(Sadjf[,2], Tadjf[,2])
ctd2 <- as.ctd(Sadj[,2][accept], Tadj[,2][accept], padj[,2][accept])
accept <- flagIsAcceptable(Sadjf[,3], Tadjf[,3])
ctd3 <- as.ctd(Sadj[,3][accept], Tadj[,3][accept], padj[,3][accept])
plotProfile(ctd1, xtype="S", type="p", pch=20)
plotProfile(ctd2, xtype="S", type="p", pch=20)
plotProfile(ctd3, xtype="S", type="p", pch=20)
plotProfile(ctd1, xtype="T", type="p", pch=20)
plotProfile(ctd2, xtype="T", type="p", pch=20)
plotProfile(ctd3, xtype="T", type="p", pch=20)
plotProfile(ctd1, xtype="sigmaTheta", type="p", pch=20)
plotProfile(ctd2, xtype="sigmaTheta", type="p", pch=20)
plotProfile(ctd3, xtype="sigmaTheta", type="p", pch=20)
plotTS(ctd1)
plotTS(ctd2)
plotTS(ctd3)


