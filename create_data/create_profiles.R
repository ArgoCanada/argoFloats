library(argoFloats)

## these blocks are FALSE because we do not want the resultant files to be
## changed if the code is rerun later.

if (FALSE) {
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

if (FALSE) {
    ## First profile of most frequent delayed-mode case in data(index)
    library(argoFloats)
    data(index)
    s1 <- subset(index, mode="delayed")
    t <- table(s1[["ID"]])
    ID <- names(t[which.max(t)])           # 4900785
    s2 <- subset(s1, ID=ID)
    profile <- s2[["profile"]][1]
    s3 <- subset(s2, profile=profile)
    stopifnot(s3[["file"]] == "aoml/4900785/profiles/D4900785_048.nc")
    p <- getProfiles(s3)
    file <- p[["file"]]
    system(paste("cp", file, "."))
    system(paste("cp", file, "../inst/extdata")) # official location
}

