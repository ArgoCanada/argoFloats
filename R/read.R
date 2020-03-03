## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Read argo profiles, creating a list of argo objects
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
#' @return [readProfiles] returns a list of `argo` objects,
#' created by [oce::read.argo()], the documentation for which
#' provides information on what can be done with such objects.
#'
#' @examples
#'\dontrun{
#' library(argoFloats)
#' library(oce)
#' # TS diagram using Argo profiles near Sable Island.
#' index <- getIndex(destdir="~/data/argo")
#' # The next line yields 63 profiles as of February 2020
#' indexSI <- subset(index, circle=list(longitude=-59.915, latitude=44.934, radius=180))
#' profilesSI <- getProfiles(indexSI)
#' argosSI <- readProfiles(profilesSI)
#' for (i in seq_len(length(argosSI))) {
#'     argo <- argosSI[[i]]
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
    if (is.character(profiles)) {
        argoFloatsDebug(debug, "case 1: vector of character strings\n")
        res <- lapply(profiles, read.argo, debug=debug-1)
    } else if (inherits(profiles, "argoFloats")) {
        type <- profiles[["type"]]
        if (type == "profiles") {
            argoFloatsDebug(debug, "case 3: object created by getProfiles()\n")
            fileNames <- gsub(".*/(.*).nc", "\\1.nc", profiles@data$file)
            fullFileNames <- paste0(profiles@metadata$destdir, "/", fileNames)
            argoFloatsDebug(debug, "about to read", length(fullFileNames), "netcdf files...\n")
            res <- lapply(fullFileNames, read.argo, debug=debug-1)
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

