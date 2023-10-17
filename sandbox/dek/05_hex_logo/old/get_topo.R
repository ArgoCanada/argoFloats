## This started failing about 2020 may 20, owing to a change in the NOAA
## server.  However, we have the topo file, so if the domain does not change,
## the logo R files will continue to work.
if (FALSE) {
    library(dc)
    topoFile <- dc.topo(-81, -72, 22, 31, resolution=1, force=TRUE)
    ## topo <- read.topo("topo_81W_72W_22N_31N_1min_gmt.nc")
    ## data: -78.97986 -74.84214  24.73996  28.44004
}

