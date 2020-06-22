library(argoFloats)
options(warn=1)
data(indexSynthetic)
n <- 500
s <- subset(index, 1:n)
p <- getProfiles(s)
a <- readProfiles(p)

cat("\n\n\n")
cat("+--------------------------------------------------+\n")
cat("| 1. Discover names of things in first netcdf file |\n")
cat("+--------------------------------------------------+\n")
cat("\n\n\n")
f <- a[[1]][["filename"]]
library(ncdf4)
n <- nc_open(f)
print(n)
nc_close(n)

cat("\n\n\n")
cat("+-----------------------------+\n")
cat("| 2. Table of some properties |\n")
cat("+-----------------------------+\n")
cat("\n\n\n")

filename <- sapply(a[["profile"]], function(x) gsub("^.*/argo/", "", x[["filename"]][1]))
df <- data.frame(filename=filename,
                 isRealtime=grepl("^[SM]{0,1}R", filename),
                 dataMode=sapply(a[["profile"]], function(x) x[["dataMode"]][1]),
                 allNAp=sapply(a[["profile"]], function(x) all(is.na(x[["pressureAdjusted"]]))),
                 allNAS=sapply(a[["profile"]], function(x) all(is.na(x[["salinityAdjusted"]]))),
                 allNAT=sapply(a[["profile"]], function(x) all(is.na(x[["temperatureAdjusted"]]))),
                 allNAO=sapply(a[["profile"]], function(x) all(is.na(x[["oxygenAdjusted"]]))),
                 havePDM=unlist(lapply(a[["profile"]], function(x) !is.null(x[["PARAMETER_DATA_MODE"]]))))
options(width=150)
print(df)
