library(oce)
library(ncdf4)
for (file in c("D4901052_069.nc", "D5901602_157.nc")) {
    cat("file: ", file, "\n")
    n <- nc_open(file)
    cat("DATA_MODE: ", ncvar_get(n, "DATA_MODE"), "\n")
    # Infer number of profiles pressure data.
    p <- ncvar_get(n, "PRES")
    str(p)
    # Are the parameter names in synch with the number of profiles.
    print(ncvar_get(n, "PARAMETER"))
    d <- read.argo(file)
    summary(d)
}

