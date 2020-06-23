library(argoFloats)
options(warn=1)
data(indexSynthetic)
n <- 500
s <- subset(subset(index, 1:n), 172:177) # see notes below
p <- getProfiles(s)
a <- readProfiles(p)



cat("\n\n\n")
cat("+-----------------------------+\n")
cat("| 1. Table of some properties |\n")
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

cat("\n\n\n")
cat("+-------------------------------------------------------------+\n")
cat("| 2. Summaries: why are i=1:3 similar, then is 4:6 different? |\n")
cat("+-------------------------------------------------------------+\n")
cat("\n\n\n")

for (i in seq_len(a[["length"]])) {
    cat("\n------------------------------------------------\n")
    cat("i = ", i, "\n")
    summary(a[[i]])
    cat("\n------------------------------------------------\n")
}
