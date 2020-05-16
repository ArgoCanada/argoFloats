## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Read argo profiles from local files
#'
#' This works with either a list of local (netCDF) files,
#' or a [`argoFloats-class`] object of type `"profiles"`, as
#' created by [getProfiles()].
#'
#' @param profiles either a character vector holding the names
#' of local files to read, or (better) an [`argoFloats-class`] object created
#' by [getProfiles()].
#' @param handleFlags a logical value (with default `TRUE`) that
#' indicates whether to call [oce::handleFlags()] on the individual argo
#' files that are to be read. This cleans up some common errors
#' that are identified in the quality-control analysis that is usually
#' done before data are place on the argo servers.
#' @param debug an integer specifying the level of debugging. If
#' this is zero, the work proceeds silently. If it is 1,
#' a small amount of debugging information is printed.  Note that
#' `debug-1` is passed to [oce::read.argo()], which actually reads
#' the file, and so it will print messages if `debug` exceeds 1.
#'
#' @return [readProfiles] returns an [`argoFloats-class`] object
#' with `type="argos"`, in which the `data` slot
#' contains a list named `argos` that holds objects
#' that are created by [oce::read.argo()].
#'
#' @examples
#' # Download and plot some profiles.
#'\dontrun{
#' library(argoFloats)
#' data(index)
#' index2 <- subset(index, 1:2)
#' profiles <- getProfiles(index2)
#' argos <- readProfiles(profiles, handleFlags=TRUE)
#' par(mfrow=c(1, 2))
#' library(oce)
#' for (i in 1:2) {
#'   A <- argos[["profile", i]]
#'   filename <- gsub(".*/", "", A[["filename"]])
#'   plotTS(A, eos="unesco")
#'   mtext(filename, line=0.7, cex=par("cex"))
#' }
#'}
#'
#' @importFrom oce handleFlags read.argo
#' @importFrom ncdf4 nc_version
#'
#' @export
#'
#' @author Dan Kelley
readProfiles <- function(profiles, handleFlags=TRUE, debug=0)
{
    debug <- floor(0.5 + debug)
    debug <- max(0, debug)
    res <- NULL
    argoFloatsDebug(debug, "readProfiles() {\n", style="bold", sep="", unindent=1)
    ## show the ncdf4 version.  Frankly, this is just to prevent errors in R CMD check.  The problem
    ## has to do with oce::read.argo() doing a require(ncdf4), which causes an error message in
    ## checking argoFloats.  But if we put argoFloats in the "Depends" field of the argoFloats
    ## DESCRIPTION file, we get an error because ArgoFloats is not using it.
    ncversion <- nc_version()
    argoFloatsDebug(debug, "ncdf4 version: ", ncversion, "\n")

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
            res@data$argos <- lapply(fullFileNames, oce::read.argo, debug=debug-1)
            argoFloatsDebug(debug, "... finished reading", length(fullFileNames), "netcdf files.\n")
        } else {
            stop("'profiles' must be a character vector or an object created by getProfiles()")
        }
    } else {
        stop("'profiles' must be a character vector or an object created by getProfiles().")
    }
    if (handleFlags) {
        for (i in seq_along(res@data$argos)) {
            res@data$argos[[i]] <- oce::handleFlags(res@data$argos[[i]])
        }
    }
    argoFloatsDebug(debug, "} # readProfiles\n", style="bold", sep="", unindent=1)
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

