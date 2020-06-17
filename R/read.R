## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

# utility file, not exported (since it may go into 'oce', where it belongs)

argoUseAdjusted <- function(argo, debug=0)
{
    if (!requireNamespace("oce", quietly=TRUE))
        stop("must install.packages(\"oce\") for argoUseAdjusted() to work")
    argoFloatsDebug(debug, "argoUseAdjusted() {\n", style="bold", sep="", unindent=1)
    res <- argo
    namesData <- names(argo@data)
    basenames <- subset(namesData, !grepl("Adjusted", namesData))
    convert <- list()
    for (basename in basenames) {
        w <- grep(basename, namesData)
        related <- namesData[w]
        if (length(related) > 1) {
            for (r in related) {
                convert[r] <- if (grepl("Adjusted", r)) gsub("Adjusted", "", r) else paste0(basename, "Unadjusted")
            }
        }
    }
    namesConvert <- names(convert)
    ## Rename data
    namesData <- names(argo@data)
    tmp <- namesData
    for (i in seq_along(tmp)) {
        w <- which(namesData[i] == namesConvert)
        if (length(w))
            tmp[i] <- convert[[w]]
    }
    argoFloatsDebug(debug, "data ORIG:  ", paste(names(argo@data), collapse=" "), "\n")
    names(res@data) <- tmp
    argoFloatsDebug(debug, "data AFTER: ", paste(names(res@data), collapse=" "), "\n")
    ## Rename metadata$flags
    namesFlags <- names(argo@metadata$flags)
    tmp <- namesFlags
    for (i in seq_along(tmp)) {
        w <- which(namesFlags[i] == namesConvert)
        if (length(w))
            tmp[i] <- convert[[w]]
    }
    argoFloatsDebug(debug, "flags ORIG:  ", paste(names(argo@metadata$flags), collapse=" "), "\n")
    names(res@metadata$flags) <- tmp
    argoFloatsDebug(debug, "flags AFTER: ", paste(names(res@metadata$flags), collapse=" "), "\n")
    ## Rename metadata$units
    namesUnits <- names(argo@metadata$units)
    tmp <- namesUnits
    for (i in seq_along(tmp)) {
        w <- which(namesUnits[i] == namesConvert)
        if (length(w))
            tmp[i] <- convert[[w]]
    }
    argoFloatsDebug(debug, "units ORIG:  ", paste(names(argo@metadata$units), collapse=" "), "\n")
    names(res@metadata$units) <- tmp
    argoFloatsDebug(debug, "units AFTER: ", paste(names(res@metadata$units), collapse=" "), "\n")
    ## Rename metadata$dataNamesOriginal
    namesUnits <- names(argo@metadata$dataNamesOriginal)
    tmp <- namesUnits
    for (i in seq_along(tmp)) {
        w <- which(namesUnits[i] == namesConvert)
        if (length(w))
            tmp[i] <- convert[[w]]
    }
    argoFloatsDebug(debug, "dataNamesOriginal ORIG:  ", paste(names(argo@metadata$dataNamesOriginal), collapse=" "), "\n")
    names(res@metadata$dataNamesOriginal) <- tmp
    argoFloatsDebug(debug, "dataNamesOriginal AFTER: ", paste(names(res@metadata$dataNamesOriginal), collapse=" "), "\n")
    res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    argoFloatsDebug(debug, "} # argoUseAdjusted()\n", style="bold", sep="", unindent=1)
    res
}




#' Read argo profiles from local files
#'
#' This works with either a list of local (netCDF) files,
#' or a [`argoFloats-class`] object of type `"profiles"`, as
#' created by [getProfiles()].  By default, warnings are issued about any
#' profiles in which 10 percent or more of the measurements are flagged
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
#' If `adjusted` is `TRUE`, then the data elements are renamed after
#' reading, so that e.g. the data named `TEMP_ADJUSTED` and
#' `TEMP_ADJUSTED_ERROR` in the source netcdf file would be
#' stored with names `temperature` and `temperatureAdjusted`,
#' with `TEMP` being stored as `temperatureUnadjusted`.  This
#' applies to all variables, not just temperature.  The quality-control
#' flags are similarly named. Note that the original names are left
#' intact, so that with
#'```
#' a <- readProfiles(..., adjusted=TRUE)
#'```
#' the values of `a[["temperature"]]` and `a[["TEMP_ADJUSTED"]]`
#' will be identical.
#'
#' @param profiles either a character vector holding the names
#' of local files to read, or (better) an [`argoFloats-class`] object created
#' by [getProfiles()].
#' @param handleFlags an optional logical value that is set to `FALSE`
#' if not provided (with a message being indicated to that effect).
#' See \dQuote{Details}.
#' @template silent
#' @param adjusted a logical value (`FALSE` by default) that indicates
#' whether to focus on the "adjusted" versions of data and quality-control
#' flags, renaming the other versions with names ending in `Unadjusted`.
#' See \dQuote{Details}.
#' @param FUN a function that reads the netcdf files in which the argo
#' profiles are stored.  If `FUN` not provided, then it defaults
#' to [oce::read.argo()].  Only experts should consider anything
#' other than this default, or a wrapper to it.
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
## @importFrom oce handleFlags read.argo
## @importFrom ncdf4 nc_version
#'
#' @export
#'
#' @author Dan Kelley
readProfiles <- function(profiles, handleFlags, adjusted=FALSE, FUN, silent=FALSE, debug=0)
{
    if (!requireNamespace("oce", quietly=TRUE))
        stop("must install.packages(\"oce\") for readProfiles() to work")
    if (!requireNamespace("ncdf4", quietly=TRUE))
        stop("must install.packages(\"ncdf4\") for readProfiles() to work")
    debug <- floor(0.5 + debug)
    debug <- max(0, debug)
    res <- NULL
    argoFloatsDebug(debug, "readProfiles() {\n", style="bold", sep="", unindent=1)
    if (missing(handleFlags)) {
        handleFlags <- FALSE
        message("readProfiles() is setting handleFlags=FALSE, so all data (not just those flagged as 'good') are retained")
    }
    if (missing(FUN)) {
        FUN <- oce::read.argo
    } else {
        if (!is.function(FUN))
            stop("FUN must be a function, e.g. read.argo")
    }
    ## show the ncdf4 version.  Frankly, this is just to prevent errors in R CMD check.  The problem
    ## has to do with oce::read.argo() doing a require(ncdf4), which causes an error message in
    ## checking argoFloats.
    ncversion <- ncdf4::nc_version()
    argoFloatsDebug(debug, "ncdf4 version: ", ncversion, "\n")

    res <- new("argoFloats", type="argos")
    if (is.character(profiles)) {
        argoFloatsDebug(debug, "case 1: vector of character strings\n")
        fileExists <- sapply(profiles, file.exists)
        if (any(!fileExists))
            stop("cannot find the following files: \"", paste(profiles[!fileExists], collapse="\", \""), "\"")
        argoFloatsDebug(debug, "reading", length(profiles), "netcdf files.\n")
        res@data$argos <- lapply(profiles, FUN, debug=debug-1)
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
                                                            default=c(0, 3, 4, 9))
            res@data$argos[[i]]@processingLog <- oce::processingLogAppend(res@data$argos[[i]]@processingLog,
                                                                     "override existing flagScheme to be mapping=list(not_assessed=0, passed_all_tests=1, probably_good=2, probably_bad=3, bad=4, changed=5, not_used_6=6, not_used_7=7, estimated=8, missing=9)),  default=c(0, 3, 4, 9)")
        }
        if (adjusted) {
            argoFloatsDebug(debug, "applying the 'adjusted' argument.\n")
            for (i in seq_len(n)) {
                res@data$argos[[i]] <- argoUseAdjusted(res@data$argos[[i]], debug=debug)
            }
        }
    } else if (inherits(profiles, "argoFloats")) {
        type <- profiles[["type"]]
        if (type == "profiles") {
            argoFloatsDebug(debug, "case 3: object created by getProfiles()\n")
            fileNames <- gsub(".*/(.*).nc", "\\1.nc", profiles@data$file)
            fullFileNames <- paste0(profiles@metadata$destdir, "/", fileNames)
            argoFloatsDebug(debug, "reading", length(fullFileNames), "netcdf files ...\n")
            res@data$argos <- lapply(fullFileNames, oce::read.argo, debug=debug-1)
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
                                                                default=c(0, 3, 4, 9))
                res@data$argos[[i]]@processingLog <- oce::processingLogAppend(res@data$argos[[i]]@processingLog,
                                                                              "override existing flagScheme to be mapping=list(not_assessed=0, passed_all_tests=1, probably_good=2, probably_bad=3, bad=4, changed=5, not_used_6=6, not_used_7=7, estimated=8, missing=9)),  default=c(0, 3, 4, 9)")
             }
            if (adjusted) {
                argoFloatsDebug(debug, "applying the 'adjusted' argument.\n")
                for (i in seq_len(n)) {
                    res@data$argos[[i]] <- argoUseAdjusted(res@data$argos[[i]], debug=debug)
                }
            }
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
    res@processingLog <- oce::processingLogAppend(res@processingLog,
                                                  paste0("readProfiles(..., handleFlags=",
                                                         handleFlags, ", adjusted=",
                                                         adjusted, ", ...)"))
    res
}

