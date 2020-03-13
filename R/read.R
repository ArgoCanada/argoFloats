## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Read argo profiles from local files
#'
#' This works with either a list of local (netCDF) files,
#' or a [argoFloats-class] object of type `"profiles"`, as
#' created by [getProfiles()].
#'
#' @param profiles either a character vector holding the names
#' of local files to read, or (better) an [argoFloats-class] object created
#' by [getProfiles()].
#' @param debug an integer specifying the level of debugging. If
#' this is zero, the work proceeds silently. If it is 1,
#' a small amount of debugging information is printed.  Note that
#' `debug-1` is passed to [oce::read.argo()], which actually reads
#' the file, and so it will print messages if `debug` exceeds 1.
#'
#' @return [readProfiles] returns an [argoFloats-class] object
#' with `type="argo"`, in which the `data` slot
#' contains a list named `argos` that holds objects
#' that are created by [oce::read.argo()].
#'
#' @examples
#'\dontrun{
#' library(argoFloats)
#' library(oce)
#' # TS diagram using Argo profiles near Sable Island.
#' indexAll <- getIndex(destdir="~/data/argo")
#' # The next line yields 63 profiles as of February 2020
#' index <- subset(indexAll, circle=list(longitude=-59.915, latitude=44.934, radius=180))
#' profiles <- getProfiles(index)
#' argos <- readProfiles(profiles)
#' # Extract and plot
#' for (i in seq_len(argos[["profile count"]])) {
#'     argo <- argos[["profile", i]]
#'     if (i == 1) {
#'         plotTS(argo, Slim=c(31, 36), Tlim=c(1,24), eos="gsw")
#'     } else {
#'         SA <- argo[["SA"]]
#'         CT <- argo[["CT"]]
#'         points(SA, CT, col=i%%10)
#'     }
#' }
#'}
#'
#' @importFrom oce read.argo
#'
#' @export
#'
#' @author Dan Kelley
readProfiles <- function(profiles, debug=0)
{
    debug <- floor(0.5 + debug)
    debug <- max(0, debug)
    res <- NULL
    argoFloatsDebug(debug, "readProfiles() {\n", style="bold", sep="", unindent=1)
    res <- new("argoFloats", type="argos")
    if (is.character(profiles)) {
        argoFloatsDebug(debug, "case 1: vector of character strings\n")
        res@data$argos <- lapply(profiles, read.argo, debug=debug-1)
    } else if (inherits(profiles, "argoFloats")) {
        type <- profiles[["type"]]
        if (type == "profiles") {
            argoFloatsDebug(debug, "case 3: object created by getProfiles()\n")
            fileNames <- gsub(".*/(.*).nc", "\\1.nc", profiles@data$file)
            fullFileNames <- paste0(profiles@metadata$destdir, "/", fileNames)
            argoFloatsDebug(debug, "about to read", length(fullFileNames), "netcdf files...\n")
            res@data$argos <- lapply(fullFileNames, read.argo, debug=debug-1)
            argoFloatsDebug(debug, "... finished reading", length(fullFileNames), "netcdf files.\n")
        } else {
            stop("'profiles' must be a character vector or an object created by getProfiles()")
        }
    } else {
        stop("'profiles' must be a character vector or an object created by getProfiles().")
    }
    argoFloatsDebug(debug, "} # readProfiles\n", style="bold", sep="", unindent=1)
    res
}

