## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Switch [[ Focus to Adjusted data (version 1: will be removed soon)
#'
#' This function returns a version of `x` for which the enclosed
#' [oce::argo-class] objects are modified in a way that makes
#' future uses of \code{\link{[[,argoFloats-method}}
#' return the *adjusted* data, not the original data.
#' **NOTE:** For this to work, the version 1.3.0 or greater
#' of \CRANpkg{oce} must be installed.
#'
#' If the version of \CRANpkg{oce} is lower than 1.3.0, `useAdjusted()`
#' reports an error.  Otherwise, it carries out its work by
#' applying `preferAdjusted()` from the \CRANpkg{oce} package to each
#' of the Argo objects stored within the `data` slot of `x`.
#'
#' Although one might guess that adjusted data
#' are always be preferable to original data, this is not always
#' the case.  Indeed, it is common for the adjusted data to consist
#' entirely of `NA` values.  A careful analyst should study both
#' data streams, and should also read the processing notes
#' for any float (or float cycle) that is of particular interest.
#' See Reference 1 for more information on the process of
#' adding adjusted data to Argo files.
#'
#' \if{html}{\figure{useAdjustedDiagram.png}{options: width=455px alt="Figure: useAdjustedDiagram.png" fig.cap="hello"}}
#'
#' @param argo an [`argoFloats-class`] object, as read by [readProfiles()].
#'
#' @param which a character vector (passed directly to
#' `preferAdjusted()`, or its mimic) naming the items for which
#' (depending also on the value of `fallback`) adjusted values
#' are to be sought by future calls to \code{\link{[[,argoFloats-method}}.
#' The short names are used, e.g. `which="oxygen"` means that
#' adjusted oxygen is to be returned in future calls
#' such as `argo[["oxygen"]]`.  The default,
#' `"all"`, means to  use adjusted values for any item in `argo`
#' that has adjusted values.
#'
#' @param fallback a logical value (passed directly to
#' `preferAdjusted()` or its mimic) indicating whether to fall back
#' to unadjusted values for any data field in which the
#' adjusted values are all `NA`.  The default value, `TRUE`,
#' avoids a problem with biogeochemical fields, where adjustment
#' of any one field may lead to insertion of "adjusted" values for
#' other fields that consist of nothing more than `NA`s.
##'
#' @param debug an integer that, if positive, indicates that some debugging information
#' should be printed.
#'
#' @examples
#' library(argoFloats)
#' # Note that useAdjusted() requires oce version to be 1.3.0 or higher.
#' if (packageVersion("oce") >= "1.3.0") {
#'     raw <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"))
#'     adj <- useAdjusted(raw)
#'     par(mfrow=c(1,2), mar=c(5,4,1,2))
#'     hist(raw[[1]][["oxygen"]], xlab="Raw Oxygen", ylab="Frequency", main=NULL)
#'     hist(adj[[1]][["oxygen"]], xlab="Adjusted Oxygen", ylab="Frequency", main=NULL)}
#'
#' @references
#' 1. Carval, Thierry, Bob Keeley, Yasushi Takatsuki, Takashi Yoshida, Stephen Loch Loch,
#' Claudia Schmid, and Roger Goldsmith. Argo User’s Manual V3.3. Ifremer, 2019.
#' \doi{10.13155/29825}
#'
#' @importFrom utils packageVersion
## @export
#'
#' @author Dan Kelley and Jaimie Harbin
useAdjustedOLD <- function(argo, which="all", fallback=TRUE, debug=0)
{
    argoFloatsDebug(debug, "useAdjustedOLD() {\n", sep="", unindent=1, style="bold")
    if (!inherits(argo, "argoFloats"))
        stop("useAdjusted() is only for argoFloats objects")
    if ("argos" != argo[["type"]])
        stop("useAdjusted() is only for argoFloats objects created by readProfiles()")
    if (packageVersion("oce") < "1.3.0")
        stop("useAdjusted() requires the oce version to be 1.3.0 or higher.")
    res <- argo
    for (i in argo[["length"]]) {
        res@data$argos[[i]] <- oce::preferAdjusted(argo@data$argos[[i]], which=which, fallback=fallback)
    }
    argoFloatsDebug(debug, "} # useAdjustedOLD()\n", sep="", unindent=1, style="bold")
    res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

#
# useAdjusted Version 2 and 3: support function
#
useAdjustedSingle <- function(argo, fallback="NA", debug=0)
{
    if (!(inherits(argo, "oce") && inherits(argo, "argo")))
        stop("First argument must be an oce::argo object")
    argoFloatsDebug(debug, "useAdjustedSingle(..., fallback=\"", fallback, "\", debug=", debug, ") {\n", sep="", unindent=1, style="bold")
    res <- argo
    fn <- argo[["filename"]]
    argoFloatsDebug(debug, "filename: \"", fn, "\"\n", sep="")
    typeFromFilename <- switch(substring(gsub(".*/","",fn),1,1), "A"="adjusted", "D"="delayed", "R"="realtime")
    varNames <- names(argo[["data"]])
    adjustedNames <- grepl("Adjusted$", varNames)
    varNamesRaw <- varNames[!adjustedNames]
    varNamesAdjusted <- varNames[adjustedNames]
    if (debug > 1) {
        argoFloatsDebug(debug, "varNames: ", paste(varNames, collapse=" "), "\n", sep="")
        argoFloatsDebug(debug, "varNamesRaw: ", paste(varNamesRaw, collapse=" "), "\n", sep="")
        argoFloatsDebug(debug, "varNamesAdjusted: ", paste(varNamesAdjusted, collapse=" "), "\n", sep="")
    }
    nrow <- nrow(argo@data$pressure)
    ncol <- ncol(argo@data$pressure)
    argoFloatsDebug(debug, "pressure is a ", ncol, "x", nrow, " matrix\n", sep="")
    stationParameters <- argo@metadata$stationParameters
    argoFloatsDebug(debug, "next is stationParameters\n")
    if (debug > 0)
        print(stationParameters)
    if ("dataMode" %in% names(argo@metadata)) {
        dm <- strsplit(argo@metadata$dataMode, "")[[1]]
        if (any(!dm %in% c("R", "A", "D"))) {
            warning("skipping a cycle, because some dataMode values are not \"R\", \"A\" or \"D\"\n")
            return(argo)
        }
        argoFloatsDebug(debug, "this cycle is of 'core' type with dm=", paste(dm, collapse=" "), "\n")
        for (name in varNamesRaw) {
            adjustedName <- paste0(name, "Adjusted")
            # There should always be an Adjusted field, but we check to be safe.
            if (adjustedName %in% varNamesAdjusted) {
                # Copy <PARAM>Adjusted into <PARAM> if any of the following is true.
                # Case 1. fallback is "NA" (irrespective of mode)
                # Case 2. mode is "A" or "D" (irrespective of fallback)
                # Case 3. fallback is "raw", mode is "R", and <PARAM>Adjusted contains some finite values
                for (icol in seq_len(ncol)) {
                    profileMode <- dm[icol]
                    case <- if (fallback == "NA") {
                        1
                    } else if (profileMode %in% c("A", "D")) {
                        2
                    } else if (profileMode == "R" && any(is.finite(argo@data[[adjustedName]][,icol]))) {
                        3
                    } else {
                        0
                    }
                    if (case > 0) {
                        res@data[[name]][,icol] <- argo@data[[adjustedName]][,icol]
                        res@metadata$flags[[name]][,icol] <- argo@metadata$flags[[adjustedName]][,icol]
                        argoFloatsDebug(debug, "  Copied \"", adjustedName, "\" to \"", name, "\" for profile ", icol, ", which is of case ", case, "\n", sep="")
                    }
                }
            }
        }
    } else if ("parameterDataMode" %in% names(argo@metadata)) {
        pdm <- argo@metadata$parameterDataMode
        argoFloatsDebug(debug, "this cycle is of not of 'core' type\n")
        warning("not doing anything with non-core data (YET)\n")
    }  else {
        warning("oce::argo object's metadata lacks both 'dataMode' and'parameterDataMode', so returning unchanged input")
        return(argo)
    }
    res@processingLog <- oce::processingLogAppend(res@processingLog,
                                                  paste0("useAdjustedSingle(argo, fallback=\"", fallback, "\", debug=", debug, ")\n"))
    argoFloatsDebug(debug, "} # useAdjustedSingle()\n", sep="", unindent=1, style="bold")
    res
}

#
# useAdjusted Version 2 and 3: driver function
#
#' Switch [[ and Plot to Focus on Adjusted data
#'
#' This function returns a copy of its first argument that has been modified by
#' (optionally) replacing 'raw' parameter values, and their flags, with
#' corresponding 'adjusted' values. The purpose is to ensure that future calls
#' to \code{\link{[[,argoFloats-method}} and plotting methods will focus on
#' the adjusted data instead of the raw data.  The procedure hinges on the value
#' of the `fallback` argument.
#'
#' There are two cases, depending on the value of `fallback`.
#' 1. If `fallback` is `"NA"` (the default), then
#' the adjusted values become the focus, even if they are all `NA`. Thus,
#' `fallback="NA"` may be preferred as part of a cautious analysis that
#' focuses only on data of high quality. The downside of this approach is a
#' reduction in coverage, since it eliminates the 'raw' fields that are typical
#' of real-time mode data components (see  Section 2.2.5 of Reference 1).
#'
#' 2. If `fallback` is `"raw"`, then the data in the adjusted parameter
#' data columns (corresponding to profiles) of each cycle are examined one by
#' one. If the values in a given column are all `NA`, then the parameter's raw
#' values for that column are not altered. However, if any of the values in a
#' given adjusted column are non-`NA`, then that entire 'adjusted' column is
#' copied into the corresponding 'raw' column. Since the `fallback="raw"`
#' scheme combines 'raw' and 'adjusted' data, it increases coverage, at the
#' possible expense of lowering overall data quality.
#'
#' @param argos an [`argoFloats-class`] object, as read by [readProfiles()].
#'
#' @param fallback a character value indicating what to do if all the adjusted
#' values for a particular parameter-profile pair are `NA`. The choices are
#' `"NA"` and `"raw"`; see \dQuote{Details}.
#'
#' @param debug an integer that controls whether debugging information is printed
#' during processing.  If `debug` is 0 or less, then no information is printed.
#' If it is 1 then minimal overall information is printed.  If it exceeds 1,
#' then information is printed for each Argo cycle contained in `argos`.
#'
#' @examples
#' library(argoFloats)
#' file <- "SD5903586_001.nc"
#' A <- readProfiles(system.file("extdata", file, package="argoFloats"))
#' B <- useAdjusted(A, "NA")
#' C <- useAdjusted(A, "raw")
#' # Notice that the original values are smaller than the adjusted values
#' plot(C, which="profile", profileControl=list(parameter="oxygen"), pch=1)
#' points(A[[1]][["oxygen"]], A[[1]][["pressure"]], pch=2)
#' points(B[[1]][["oxygen"]], B[[1]][["pressure"]], pch=3)
#' legend("bottomright", pch=c(3,1,2), legend=c("fallback='NA'", "fallback='raw'", "original"))
#'
#' @author Dan Kelley and Jaimie Harbin
#'
#' @references
#' 1. Argo Data Management Team. "Argo User’s Manual V3.4,"
#' January 20, 2021. <https://archimer.ifremer.fr/doc/00187/29825/>
#'
#' @export
useAdjusted <- function(argos, fallback="NA", debug=0)
{
    argoFloatsDebug(debug, "useAdjusted(..., fallback=\"", fallback, "\", debug=", debug, ") {\n", sep="", unindent=1, style="bold")
    if (!inherits(argos, "argoFloats"))
        stop("'argos' must be an argoFloats object")
    if ("argos" != argos@metadata$type)
        stop("'argos' must be an argoFloats object created with argoFloats::readProfiles()")
    if (!fallback %in% c("NA", "raw"))
        stop("fallback value \"", fallback, "\" is not understood.  It must be \"NA\" or \"raw\"")
    res <- argos
    argoList <- argos[["argos"]]
    for (i in seq_along(argoList)) {
        res@data$argos[[i]] <- useAdjustedSingle(argoList[[i]], fallback=fallback, debug=debug-1)
    }
    res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    argoFloatsDebug(debug, "} # useAdjusted()\n", sep="", unindent=1, style="bold")
    res
}

