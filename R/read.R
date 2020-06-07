## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Read argo profiles from local files
#'
#' This works with either a list of local (netCDF) files,
#' or a [`argoFloats-class`] object of type `"profiles"`, as
#' created by [getProfiles()].  By default, warnings are issued about any
#' profiles in which 50 percent or more of the measurements are flagged
#' with a quality-control code of 4 (which designates bad data),
#' and these values are set to `NA`; use the `silent` and `handleFlags`
#' arguments to control this behaviour.
#'
#' If `handleFlags` is `TRUE`, then the [oce::handleFlags()] function
#' from the \CRANpkg{oce} package is called on each individual argo object
#' that is read by [oce::read.argo()] in that package.  The action
#' of [oce::handleFlags()] is see whether any data are flagged
#' with the quality-control code that is not equal to 1, and to set
#' the corresponding data to `NA`.  In the `oce` terminology,
#' the flag meanings are:
#' `not_assessed`=0, `passed_all_tests`=1, `probably_good`=2, `probably_bad`=3,
#' `bad`=4, `changed`=5, `averaged`=7, `interpolated`=8, `missing`=9, and so
#' the restriction to only values equal to 1 means that multiple categories
#' of potentially useable data are discarded, and for that reason,
#' `handleFlags` is set to `FALSE` unless the user provides a value in
#' the call to `readProfiles()`.  See Wong et al. (2020) for a discussion
#' of flags in argo data.
#'
#' @param profiles either a character vector holding the names
#' of local files to read, or (better) an [`argoFloats-class`] object created
#' by [getProfiles()].
#' @param handleFlags an optional logical value that is set to `FALSE`
#' if not provided (with a message being indicated to that effect).
#' See \dQuote{Details}.
#' @template silent
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
#' @references
#'
#' Wong, Annie, Robert Keeley, Thierry Carval, and Argo Data Management Team.
#' “Argo Quality Control Manual for CTD and Trajectory Data,” January 1, 2020.
#' \url{https://archimer.ifremer.fr/doc/00228/33951}.
#' \url{http://dx.doi.org/10.13155/33951}.
#'
#' @examples
#' # Download and plot some profiles.
#'\dontrun{
#' library(argoFloats)
#' data(index)
#' index1 <- subset(index, 1)
#' profiles <- getProfiles(index1)
#' argosWithNA<- readProfiles(profiles, handleFlags=FALSE)
#' argosWithoutNA <- readProfiles(profiles, handleFlags=FALSE)
#' par(mfrow=c(1, 2))
#' file <- gsub(".*/", "",  profiles[[1]])
#' aWithNA <- argosWithNA[[1]]
#' plotTS(aWithNA, eos="unesco", type="o")
#' mtext(paste(file, "\n handling flags"), cex=0.7*par("cex"))
#' aWithoutNA <- argosWithoutNA[[1]]
#' plotTS(aWithoutNA, eos="unesco", type="o")
#' mtext(paste(file, "\n ignoring flags"), cex=0.7*par("cex"))
#'}
#'
#' @importFrom oce handleFlags read.argo
#' @importFrom ncdf4 nc_version
#'
#' @export
#'
#' @author Dan Kelley
readProfiles <- function(profiles, handleFlags, silent=FALSE, debug=0)
{
    debug <- floor(0.5 + debug)
    debug <- max(0, debug)
    res <- NULL
    argoFloatsDebug(debug, "readProfiles() {\n", style="bold", sep="", unindent=1)
    if (missing(handleFlags)) {
        handleFlags <- FALSE
        message("readProfiles() is setting handleFlags=FALSE, so all data (not just those flagged as 'good') are retained")
    }
    ## show the ncdf4 version.  Frankly, this is just to prevent errors in R CMD check.  The problem
    ## has to do with oce::read.argo() doing a require(ncdf4), which causes an error message in
    ## checking argoFloats.  But if we put argoFloats in the "Depends" field of the argoFloats
    ## DESCRIPTION file, we get an error because ArgoFloats is not using it.
    ncversion <- ncdf4::nc_version()
    argoFloatsDebug(debug, "ncdf4 version: ", ncversion, "\n")

    res <- new("argoFloats", type="argos")
    if (is.character(profiles)) {
        argoFloatsDebug(debug, "case 1: vector of character strings\n")
        fileExists <- sapply(profiles, file.exists)
        if (any(!fileExists))
            stop("cannot find the following files: \"", paste(profiles[!fileExists], collapse="\", \""), "\"")
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
    ## tabulate flags (ignore "Adjusted" items)
    if (!silent || debug) {
        flagNamesAll <- unique(sort(unlist(lapply(res@data$argos, function(a) names(a@metadata$flags)))))
        flagNames <- flagNamesAll[!grepl("Adjusted$", flagNamesAll)]
        for (flagName in flagNames) {
            percentBad <- sapply(res@data$argos,
                                 function(x) {
                                     if (flagName %in% names(x@metadata$flags)) {
                                         100 * sum(x@metadata$flags[[flagName]]==4) / length(x@metadata$flags[[flagName]])
                                     } else {
                                         NA
                                     }
                                 })
            badCases <- percentBad > 10
            if (any(badCases, na.rm=TRUE)) {
                warning("Of ", length(badCases), " profiles read, ",
                        sum(badCases, na.rm=TRUE),
                        if (sum(badCases, na.rm=TRUE) > 1) " have " else " has ",
                        ">10% of ", flagName, " values with QC flag of 4, signalling bad data.",
                        if (handleFlags) {
                            "\n    These data are set to NA, because the handleFlags argument is TRUE"
                        } else {
                            "\n    These data are retained, because the handleFlags argument is FALSE"
                        },
                        "\n    The indices of the bad profiles are as follows.",
                        "\n    ", paste(which(badCases), collapse=" "))
            }
        }
    }
    argoFloatsDebug(debug, "} # readProfiles\n", style="bold", sep="", unindent=1)
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

