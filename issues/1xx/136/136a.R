library(argoFloats)
library(ncdf4)
library(oce)
files <- c("~/git/oce/create_data/argo/6900388_prof.nc", "~/data/argo/D3900740_193.nc")
for (file in files) {
    if (file.exists(file)) {
        a <- readProfiles(file)
        a1 <- a[[1]]
        n <- nc_open(file)
        hqt <- ncvar_get(n, "HISTORY_QCTEST")
        if (!all.equal(a1[["HISTORY_QCTEST"]], gsub("[ ]*$", "", hqt)))
            stop("'", file, "' FAILED the [[\"HISTORY_QCTEST\"]] test")
        else
            message("'", file, "' PASSED the [[\"HISTORY_QCTEST\"]] test")
    } else {
        message("'", file, "' cannot be checked, because it is not on this filesystem")
    }
}

