## Blocks are disabled once we have the files, because we do not want confusion
## with respect to the documentation, in which specific download dates are
## stated.  This prevents later changing files, which could break build-time
## unit tests and/or render the documentation incorrect.

forceUpdate <- FALSE

if (forceUpdate) {
    library(argoFloats)
    ## https://github.com/ArgoCanada/argoFloats/issues/117#issuecomment-640758567
    i <- getIndex(filename="synthetic", age=0)
    s <- subset(subset(subset(i, mode="realtime"), id=2902204), cycle=131)
    p <- getProfiles(s)
    stopifnot(s[['file']] == "incois/2902204/profiles/SR2902204_131.nc")
    cat("SOURCE: '", p[["url"]], "'\n", sep="")
    ## SOURCE: 'ftp://ftp.ifremer.fr/ifremer/argo/dac/incois/2902204/profiles/SR2902204_131.nc'
    cat("FILE:   '", p[["file"]], "'\n", sep="")
    ## FILE:   '~/data/argo/SR2902204_131.nc'
    system("cp ~/data/argo/SR2902204_131.nc .") # retain for a backup
    system("cp ~/data/argo/SR2902204_131.nc ../inst/extdata") # official location
}

if (forceUpdate) {
    library(argoFloats)
    data(index)
    s1 <- subset(index, mode="delayed")
    t <- table(s1[["id"]])
    id <- names(t[which.max(t)])           # 4900785
    s2 <- subset(s1, id=id)
    cycle <- s2[["cycle"]][1]
    s3 <- subset(s2, cycle=cycle)
    stopifnot(s3[["file"]] == "aoml/4900785/profiles/D4900785_048.nc")
    p <- getProfiles(s3)
    file <- p[["file"]]
    cat("SOURCE: '", p[["url"]], "'\n", sep="")
    ## SOURCE: 'ftp://usgodae.org/pub/outgoing/argo/dac/aoml/4900785/profiles/D4900785_048.nc'
    cat("FILE:   '", p[["file"]], "'\n", sep="")
    ## FILE:   '~/data/argo/D4900785_048.nc'
    system(paste("cp", file, "."))
    system(paste("cp", file, "../inst/extdata")) # official location
}

if (forceUpdate) {
    ## a bcg-argo file with adjusted oxygen differing from original oxygen
    library(argoFloats)
    i <- getIndex('synthetic', age=0)
    s <- subset(subset(i, id='5903586'), 1)
    p <- getProfiles(s)
    cat("SOURCE: '", p[["url"]], "'\n", sep="")
    ## SOURCE: 'ftp://ftp.ifremer.fr/ifremer/argo/dac/aoml/5903586/profiles/SD5903586_001.nc'
    cat("FILE:   '", p[["file"]], "'\n", sep="")
    ## FILE:   '~/data/argo/SD5903586_001.nc'
    system(paste("cp", p[["file"]], "."))
    system(paste("cp", p[["file"]], "../inst/extdata")) # official location
}

