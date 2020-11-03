## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Read argo profiles from local files
#'
#' This works with either a vector of netCDF files,
#' or a [`argoFloats-class`] object of type `"profiles"`, as
#' created by [getProfiles()].
#' During the reading, argo profile objects are created with [oce::read.argo()]
#' or a replacement function provided as the `FUN` argument.
#'
#' By default, warnings are issued about any
#' profiles in which 10 percent or more of the measurements are flagged
#' with a quality-control code of 0, 3, 4, 6, 7, or 9 (see the
#' [applyQC()] documentation for the meanings of these codes).
#'
#' @param profiles either (1) a character vector that holds
#' the names of netcfd files or (2) an [`argoFloats-class`]
#' object created by [getProfiles()]. In the first case, any
#' items that start with `"ftp:"` are taken to represent
#' the full paths to remote files, and these first downloaded
#' to the `destdir` directory using [getProfileFromUrl()].
#'
#' @param FUN a function that reads the netcdf files in which the argo
#' profiles are stored.  If `FUN` not provided, then it defaults
#' to [oce::read.argo()].  Only experts should consider anything
#' other than this default, or a wrapper to it.
#'
#' @param destdir a character value that specifies a directory into
#' which to save downloaded files, in the case that they are
#' provided in the `profiles` argument.
#'
#' @template quiet
#'
#' @param debug an integer specifying the level of debugging. If
#' this is zero, the work proceeds silently. If it is 1,
#' a small amount of debugging information is printed.  Note that
#' `debug-1` is passed to [oce::read.argo()], which actually reads
#' the file, and so it will print messages if `debug` exceeds 1.
#'
#' @return An [`argoFloats-class`] object
#' with `type="argos"`, in which the `data` slot
#' contains a list named `argos` that holds objects
#' that are created by [oce::read.argo()].
#'
#' @examples
#' # Example 1: read 5 profiles and plot TS for the first, in raw and QC-cleaned forms.
#'\dontrun{
#' library(argoFloats)
#' data(index)
#' index1 <- subset(index, 1)
#' profiles <- getProfiles(index1)
#' raw <- readProfiles(profiles)
#' clean <- applyQC(profiles)
#' par(mfrow=c(1, 2))
#' file <- gsub(".*/", "",  profiles[[1]])
#' aWithNA <- argosWithNA[[1]]
#' plotTS(raw[[1]], eos="unesco", type="o")
#' mtext(file, cex=0.7*par("cex"))
#' aWithoutNA <- argosWithoutNA[[1]]
#' plotTS(clean[[1]], eos="unesco", type="o")
#' mtext(paste(file, "\n (after applying QC)"), cex=0.7*par("cex"))
#'}
#'
#' # Example 2: read from a URI
#'\dontrun{
#' u <- "ftp://usgodae.org/pub/outgoing/argo/dac/aoml/5903586/profiles/BD5903586_001.nc"
#' p <- readProfiles(u)
#'}
#'
#' @export
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @author Dan Kelley
readProfiles <- function(profiles, FUN, destdir="~/data/argo", quiet=FALSE, debug=0)
{
    if (!requireNamespace("oce", quietly=TRUE))
        stop("must install.packages(\"oce\") for readProfiles() to work")
    if (!requireNamespace("ncdf4", quietly=TRUE))
        stop("must install.packages(\"ncdf4\") for readProfiles() to work")
    res <- NULL
    argoFloatsDebug(debug, "readProfiles() {\n", style="bold", sep="", unindent=1)
    if (missing(FUN)) {
        FUN <- oce::read.argo
    } else {
        if (!is.function(FUN))
            stop("FUN must be a function, e.g. oce::read.argo")
    }
    ## show the ncdf4 version.  Frankly, this is just to prevent errors in R CMD check.  The problem
    ## has to do with oce::read.argo() doing a require(ncdf4), which causes an error message in
    ## checking argoFloats.
    ncversion <- ncdf4::nc_version()
    argoFloatsDebug(debug, "ncdf4 version: ", ncversion, "\n")
    res <- new("argoFloats", type="argos")
    if (is.character(profiles)) {
        argoFloatsDebug(debug, "Case 1: vector of ", length(profiles), " character valuesn", sep="")
        n <- length(profiles)
        res@data$argos <- vector("list", length=n)
        for (i in seq_len(n)) {
            if (grepl("^ftp:", profiles[i])) {
                localFile <- getProfileFromUrl(profiles[i], destdir=destdir, debug=debug, quiet=quiet)
                res@data$argos[[i]] <- FUN(localFile, debug=debug-1)
            } else {
                argoFloatsDebug(debug, "Attempting to read file '", profiles[i], "'.\n", sep="")
                if (!file.exists(profiles[i]))
                    stop("cannot find the local file: '", profiles[i], "'")
                res@data$argos[[i]] <- FUN(profiles[i], debug=debug-1)
            }
        }
        # res@data$argos <- lapply(profiles, FUN, debug=debug-1)
        n <- length(res@data$argos)
        argoFloatsDebug(debug, "initializing the flag-mapping scheme in the profiles (over-rides oce defaults).\n")
        for (i in seq_len(n)) {
            res@data$argos[[i]]@metadata$flagScheme <- list(name="argo",
                                                            mapping=list(not_assessed=0,
                                                                         passed_all_tests=1,
                                                                         probably_good=2,
                                                                         probably_bad=3,
                                                                         bad=4,
                                                                         changed=5,
                                                                         not_used_6=6,
                                                                         not_used_7=7,
                                                                         estimated=8,
                                                                         missing=9),
                                                            default=c(0, 3, 4, 6, 7, 9))
            res@data$argos[[i]]@processingLog <- oce::processingLogAppend(res@data$argos[[i]]@processingLog,
                                                                     "override existing flagScheme to be mapping=list(not_assessed=0, passed_all_tests=1, probably_good=2, probably_bad=3, bad=4, changed=5, not_used_6=6, not_used_7=7, estimated=8, missing=9)),  default=c(0, 3, 4, 9)")
        }
    } else if (inherits(profiles, "argoFloats")) {
        type <- profiles[["type"]]
        if (type == "profiles") {
            argoFloatsDebug(debug, "case 2: object created by getProfiles()\n")
            mustSkip <- is.na(profiles@data$file)
            if (sum(mustSkip)) {
                if (sum(mustSkip) == 1) {
                    warning("skipping a profile with NA file name, at index ", which(mustSkip))
                } else {
                    warning("skipping ", sum(mustSkip), " profiles with NA file names, at indices: ",
                            paste(which(mustSkip), collapse=" "))
                }
            }
            if (all(mustSkip))
                stop("No valid files found in the \"", profiles@metadata$destdir, "\" directory. Perhaps getProfiles() was unable to download them, or they were deleted after downloading.")
            fileNames <- gsub(".*/(.*).nc", "\\1.nc", profiles@data$file[!mustSkip])
            fullFileNames <- paste0(profiles@metadata$destdir, "/", fileNames)
            argoFloatsDebug(debug, "reading", length(fullFileNames), "netcdf files ...\n")
            res@data$argos <- lapply(fullFileNames, oce::read.argo, debug=debug-1)
            n <- length(res@data$argos)
            argoFloatsDebug(debug, "initializing the flag-mapping scheme in the profiles (over-rides oce defaults).\n")
            if (!quiet)
                pb <- txtProgressBar(0, n, 0, style=3)
            for (i in seq_len(n)) {
                Sys.sleep(0.01)
                res@data$argos[[i]]@metadata$flagScheme <- list(name="argo",
                                                                mapping=list(not_assessed=0,
                                                                             passed_all_tests=1,
                                                                             probably_good=2,
                                                                             probably_bad=3,
                                                                             bad=4,
                                                                             changed=5,
                                                                             not_used_6=6,
                                                                             not_used_7=7,
                                                                             estimated=8,
                                                                             missing=9),
                                                                default=c(0, 3, 4, 9))
                res@data$argos[[i]]@processingLog <- oce::processingLogAppend(res@data$argos[[i]]@processingLog,
                                                                              "override existing flagScheme to be mapping=list(not_assessed=0, passed_all_tests=1, probably_good=2, probably_bad=3, bad=4, changed=5, not_used_6=6, not_used_7=7, estimated=8, missing=9)),  default=c(0, 3, 4, 9)")
                if (!quiet)
                    setTxtProgressBar(pb, i)
            }
            if (!quiet)
                close(pb)
        } else {
            stop("'profiles' must be a character vector or an object created by getProfiles()")
        }
    } else {
        stop("'profiles' must be a character vector or an object created by getProfiles().")
    }
    ## tabulate flags (ignore "Adjusted" items)
    if (!quiet || debug) {
        flagNamesAll <- unique(sort(unlist(lapply(res@data$argos, function(a) names(a@metadata$flags)))))
        flagNames <- flagNamesAll[!grepl("Adjusted$", flagNamesAll)]
        for (flagName in flagNames) {
            percentBad <- sapply(res@data$argos,
                                 function(x) {
                                     if (flagName %in% names(x@metadata$flags)) {
                                         nbad <- sum(x@metadata$flags[[flagName]] %in% c(0, 3, 4, 6, 7, 9))
                                         100 * nbad / length(x@metadata$flags[[flagName]])
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
                        "\n    The indices of the bad profiles are as follows.",
                        "\n    ", paste(which(badCases), collapse=" "), immediate. = TRUE)
            }
        }
    }
    argoFloatsDebug(debug, "} # readProfiles\n", style="bold", sep="", unindent=1)
    res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

