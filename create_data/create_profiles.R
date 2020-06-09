library(argoFloats)

if (TRUE) {
    message("No need to download the index and select the file, because already done")
    ## ~/data/argo/SR2902204_131.nc
    ## https://github.com/ArgoCanada/argoFloats/issues/117#issuecomment-640758567
    i <- getIndex(filename="synthetic", age=0) # Mon  8 Jun 2020 14:34:22 ADT
    s1 <- subset(i, mode="realtime")
    s2 <- subset(s1, ID=2902204)
    s3 <- subset(s2, profile=131)
    stopifnot(s3[['file']] == "incois/2902204/profiles/SR2902204_131.nc")
    system("cp ~/data/argo/SR2902204_131.nc .") # retain for a backup
    system("cp ~/data/argo/SR2902204_131.nc ../inst/extdata") # official location
}

