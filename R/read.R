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
#' # Download and plot some profiles.
#'\donttest{
#' library(argoFloats)
#' data(index)
#' index2 <- subset(index, 1:2)
#' profiles <- getProfiles(index2, destdir=".")
#' argos <- readProfiles(profiles)
#' par(mfrow=c(2, 2))
#' library(oce)
#' for (i in 1:2) {
#'   A <- argos[["profile", i]]
#'   filename <- gsub(".*/", "", A[["filename"]])
#'   plotTS(A, eos="unesco")
#'   mtext(filename, line=0.7, cex=par("cex"))
#'   Aclean <- handleFlags(A)
#'   plotTS(Aclean, eos="unesco")
#'   mtext(paste(filename, "(cleaned)"), line=0.7, cex=par("cex"))
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
    ##if (!requireNamespace("ncdf4"))
    ##    stop("please install.package(\"ncdf4\") so that readProfiles() can read argo files")
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

