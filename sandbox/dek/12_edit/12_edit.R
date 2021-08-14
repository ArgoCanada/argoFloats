## NOTE that we copy the file!!!

library(ncdf4)
library(argoFloats)
file <- system.file("extdata", "D4900785_048.nc", package="argoFloats")
## MUST copy the file, to avoid overwriting important original data!
base <- gsub(".*/(.*).nc", "\\1", file)
tmp <- paste0(base, "_edited.nc")
cmd <- paste("cp", file, tmp)
system(cmd)

n <- nc_open(tmp, write=TRUE)
## expect <- system(paste("ncdump -h", tmp), intern=TRUE)
var <- n$var
stopifnot(length(var) == n$nvar)
## "ncdump -h file" labels atts as "global attributes"
atts <- ncatt_get(n, 0)
stopifnot(length(atts) == n$natts)

PSAL_QC <- ncvar_get(n, "PSAL_QC")
cat("PSAL_QC orig: ", PSAL_QC, "\n", sep="")
TEMP_QC <- ncvar_get(n, "TEMP_QC")
cat("TEMP_QC orig: ", TEMP_QC, "\n", sep="")
## mark S and T as bad
badIndices <- 30:50
fS <- strsplit(PSAL_QC, "")[[1]]
fS[badIndices] <- "4"
PSAL_QC_new <- paste(fS, collapse="")
fT <- strsplit(TEMP_QC, "")[[1]]
fT[badIndices] <- "4"
TEMP_QC_new <- paste(fT, collapse="")

cat("TEMP_QC  new: ", TEMP_QC_new, "\n", sep="")
cat("PSAL_QC  new: ", PSAL_QC_new, "\n", sep="")

ncvar_put(n, "TEMP_QC", TEMP_QC_new)
ncvar_put(n, "PSAL_QC", PSAL_QC_new)
nc_sync(n)
nc_close(n)

orig <- readProfiles(file)
test <- readProfiles(tmp)
stopifnot(test[["salinityFlag"]][[1]][badIndices] == 4)

## df <- data.frame(orig=orig[["salinityFlag"]][[1]], test=test[["salinityFlag"]][[1]])

if (!interactive()) pdf("12_edit.pdf")
par(mfrow=c(1,2))
plot(applyQC(orig), which="TS")
mtext(gsub(".*/", "", file), line=1)
plot(applyQC(test), which="TS")
mtext(paste("after marking indices", paste(range(badIndices),collapse=" to "), "as bad"), line=1)

if (!interactive()) dev.off()

