library(argoFloats)
options(warn=1)
data(indexSynthetic)
n <- 100
s <- subset(index, 1:n)
p <- getProfiles(s)
a <- readProfiles(p)
filename <- sapply(a[["profile"]], function(x) gsub("^.*/argo/", "", x[["filename"]][1]))
isRealtime <- grepl("^[SM]{0,1}R", filename)
dataMode <- sapply(a[["profile"]], function(x) x[["dataMode"]][1])
allNAoxygen <- sapply(a[["profile"]], function(x) all(is.na(x[["oxygenAdjusted"]])))
allNApressure <- sapply(a[["profile"]], function(x) all(is.na(x[["pressureAdjusted"]])))
allNAsalinity <- sapply(a[["profile"]], function(x) all(is.na(x[["salinityAdjusted"]])))
allNAtemperature <- sapply(a[["profile"]], function(x) all(is.na(x[["temperatureAdjusted"]])))
df <- data.frame(filename=filename, isRealtime=isRealtime, dataMode=dataMode,
                 allNAp=allNApressure,
                 allNAS=allNAsalinity,
                 allNAT=allNAtemperature,
                 allNAO=allNAoxygen)
cat("Q: is pressureAdjusted is NA for all depths in all profiles?", if (all(df$allNApressure)) "YES" else "NO", "\n")
cat("Q: is salinityAdjusted is NA for all depths in all profiles?", if (all(df$allNAsalinity)) "YES" else "NO", "\n")
cat("Q: is temperatureAdjusted is NA for all depths in all profiles?", if (all(df$allNAtemperature)) "YES" else "NO", "\n")
cat("Q: is oxygenAdjusted is NA for all depths in all profiles?", if (all(df$allNA)) "YES" else "NO", "\n")
options(width=200)
print(df)

