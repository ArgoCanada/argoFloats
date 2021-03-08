## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4:tw=80

# <OLD> #' Switch [[ Focus to Adjusted data (version 1: will be removed soon)
# <OLD> #'
# <OLD> #' This function returns a version of `x` for which the enclosed
# <OLD> #' [oce::argo-class] objects are modified in a way that makes
# <OLD> #' future uses of \code{\link{[[,argoFloats-method}}
# <OLD> #' return the *adjusted* data, not the original data.
# <OLD> #' **NOTE:** For this to work, the version 1.3.0 or greater
# <OLD> #' of \CRANpkg{oce} must be installed.
# <OLD> #'
# <OLD> #' If the version of \CRANpkg{oce} is lower than 1.3.0, `useAdjusted()`
# <OLD> #' reports an error.  Otherwise, it carries out its work by
# <OLD> #' applying `preferAdjusted()` from the \CRANpkg{oce} package to each
# <OLD> #' of the Argo objects stored within the `data` slot of `x`.
# <OLD> #'
# <OLD> #' Although one might guess that adjusted data
# <OLD> #' are always be preferable to original data, this is not always
# <OLD> #' the case.  Indeed, it is common for the adjusted data to consist
# <OLD> #' entirely of `NA` values.  A careful analyst should study both
# <OLD> #' data streams, and should also read the processing notes
# <OLD> #' for any float (or float cycle) that is of particular interest.
# <OLD> #' See Reference 1 for more information on the process of
# <OLD> #' adding adjusted data to Argo files.
# <OLD> #'
# <OLD> #' \if{html}{\figure{useAdjustedDiagram.png}{options: width=455px alt="Figure: useAdjustedDiagram.png" fig.cap="hello"}}
# <OLD> #'
# <OLD> #' @param argo an [`argoFloats-class`] object, as read by [readProfiles()].
# <OLD> #'
# <OLD> #' @param which a character vector (passed directly to
# <OLD> #' `preferAdjusted()`, or its mimic) naming the items for which
# <OLD> #' (depending also on the value of `fallback`) adjusted values
# <OLD> #' are to be sought by future calls to \code{\link{[[,argoFloats-method}}.
# <OLD> #' The short names are used, e.g. `which="oxygen"` means that
# <OLD> #' adjusted oxygen is to be returned in future calls
# <OLD> #' such as `argo[["oxygen"]]`.  The default,
# <OLD> #' `"all"`, means to  use adjusted values for any item in `argo`
# <OLD> #' that has adjusted values.
# <OLD> #'
# <OLD> #' @param fallback a logical value (passed directly to
# <OLD> #' `preferAdjusted()` or its mimic) indicating whether to fall back
# <OLD> #' to unadjusted values for any data field in which the
# <OLD> #' adjusted values are all `NA`.  The default value, `TRUE`,
# <OLD> #' avoids a problem with biogeochemical fields, where adjustment
# <OLD> #' of any one field may lead to insertion of "adjusted" values for
# <OLD> #' other fields that consist of nothing more than `NA`s.
# <OLD> ##'
# <OLD> #' @param debug an integer that, if positive, indicates that some debugging information
# <OLD> #' should be printed.
# <OLD> #'
# <OLD> #' @examples
# <OLD> #' library(argoFloats)
# <OLD> #' # Note that useAdjusted() requires oce version to be 1.3.0 or higher.
# <OLD> #' if (packageVersion("oce") >= "1.3.0") {
# <OLD> #'     raw <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"))
# <OLD> #'     adj <- useAdjusted(raw)
# <OLD> #'     par(mfrow=c(1,2), mar=c(5,4,1,2))
# <OLD> #'     hist(raw[[1]][["oxygen"]], xlab="Raw Oxygen", ylab="Frequency", main=NULL)
# <OLD> #'     hist(adj[[1]][["oxygen"]], xlab="Adjusted Oxygen", ylab="Frequency", main=NULL)}
# <OLD> #'
# <OLD> #' @references
# <OLD> #' 1. Carval, Thierry, Bob Keeley, Yasushi Takatsuki, Takashi Yoshida, Stephen Loch Loch,
# <OLD> #' Claudia Schmid, and Roger Goldsmith. Argo User’s Manual V3.3. Ifremer, 2019.
# <OLD> #' \doi{10.13155/29825}
# <OLD> #'
# <OLD> #' @importFrom utils packageVersion
# <OLD> ## @export
# <OLD> #'
# <OLD> #' @author Dan Kelley and Jaimie Harbin
# <OLD> useAdjustedOLD <- function(argo, which="all", fallback=TRUE, debug=0)
# <OLD> {
# <OLD>     argoFloatsDebug(debug, "useAdjustedOLD() {\n", sep="", unindent=1, style="bold")
# <OLD>     if (!inherits(argo, "argoFloats"))
# <OLD>         stop("useAdjusted() is only for argoFloats objects")
# <OLD>     if ("argos" != argo[["type"]])
# <OLD>         stop("useAdjusted() is only for argoFloats objects created by readProfiles()")
# <OLD>     if (packageVersion("oce") < "1.3.0")
# <OLD>         stop("useAdjusted() requires the oce version to be 1.3.0 or higher.")
# <OLD>     res <- argo
# <OLD>     for (i in argo[["length"]]) {
# <OLD>         res@data$argos[[i]] <- oce::preferAdjusted(argo@data$argos[[i]], which=which, fallback=fallback)
# <OLD>     }
# <OLD>     argoFloatsDebug(debug, "} # useAdjustedOLD()\n", sep="", unindent=1, style="bold")
# <OLD>     res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
# <OLD>     res
# <OLD> }

# useAdjusted support function (not exported)
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
        dataMode <- argo@metadata$dataMode
        if (any(!dataMode %in% c("R", "A", "D"))) {
            warning("skipping a cycle, because some dataMode values are not \"R\", \"A\" or \"D\"\n")
            return(argo)
        }
        argoFloatsDebug(debug, "this cycle is of 'core' type with dataMode=", paste(dataMode, collapse=" "), "\n")
        for (name in varNamesRaw) {
            adjustedName <- paste0(name, "Adjusted")
            # There should always be an Adjusted field, but we check to be safe.
            if (adjustedName %in% varNamesAdjusted) {
                # Copy <PARAM>Adjusted into <PARAM> if either of the following is true.
                #    Case 1. fallback is FALSE
                #    Case 2. fallback is TRUE and mode is "A" or "D"
                for (icol in seq_len(ncol)) {
                    profileMode <- dataMode[icol]
                    case <- if (fallback == "NA") { 1 } else if (profileMode %in% c("A", "D")) { 2 } else { 0 }
                    if (case > 0) {
                        res@data[[name]][,icol] <- argo@data[[adjustedName]][,icol]
                        res@metadata$flags[[name]][,icol] <- argo@metadata$flags[[adjustedName]][,icol]
                        argoFloatsDebug(debug, "  Copied \"", adjustedName, "\" to \"", name, "\" for profile ", icol, ", which is of case ", case, "\n", sep="")
                    }
                }
            }
        }
    } else if ("parameterDataMode" %in% names(argo@metadata)) {
        parameterDataMode <- argo@metadata$parameterDataMode
        argoFloatsDebug(debug, "this cycle is of not of 'core' type; next is parameterDataMode:\n")
        if (debug)
            print(parameterDataMode)
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

#' Switch [[ and Plot to Focus on Adjusted data
#'
#' `useAdjusted` returns a copy of an `argos`-type object (as created with the
#' [readProfiles()] function), in which the individual
#' [oce::argo-class] objects have been modified so that future calls to
#' `[[,argoFloats-method` or the `plot,argoFloats-method()` will work with
#' *adjusted* versions of the data.  In effect, this changes the focus from the
#' observed data to adjusted data.  The works profile-by-profile, for each of
#' the [oce::argo-class] objects stored within the first argument.
#' The details of how this works are controlled by
#' the `fallback` argument.
#'
#' If `fallback` is `TRUE`, which the default, then the focus switches entirely
#' to the adjusted data.  This improves the overall reliability of the results,
#' but at the cost of eliminating realtime-mode data.  This is because the
#' adjusted fields for realtime data are set to `NA` as a matter of policy (see
#' section JJJ of reference 1).
#'
#' Wider data coverage is obtained if `fallback` is set to `FALSE`.  In this
#' case, the focus is shifted to adjusted data *only if* the data-mode for
#' the individual profiles is `A` or `D`, indicating either Adjusted or
#' Delayed mode. Any profiles that are of the `R` (Realtime) data-mode are
#' left unaltered. This blending of adjusted and unadjusted data offers
#' improved spatial and temporal coverage, while reducing the overall
#' data quality, and so this approach should be used with caution.
#'
#' @param argos an [`argoFloats-class`] object, as read by [readProfiles()].
#'
#' @param fallback a logical value indicating whether to 'fall back' from
#' adjusted data to raw data for profiles that are in real-time mode.  By
#' default, `fallback` is `FALSE`, to focus entirely on
#' adjusted data.  See \dQuote{Details}.
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
#' B <- useAdjusted(A)
#' C <- useAdjusted(A, TRUE)
#' # Notice that the original values are smaller than the adjusted values
#' plot(C, which="profile", profileControl=list(parameter="oxygen"), pch=1)
#' points(A[[1]][["oxygen"]], A[[1]][["pressure"]], pch=2)
#' points(B[[1]][["oxygen"]], B[[1]][["pressure"]], pch=3)
#' legend("bottomright", pch=c(3,1,2), legend=c("fallback='NA'", "fallback='raw'", "original"))
#'
#' @author Dan Kelley, Jaimie Harbin and Clark Richards
#'
#' @references
#' 1. Argo Data Management Team. "Argo User’s Manual V3.4,"
#' January 20, 2021. <https://archimer.ifremer.fr/doc/00187/29825/>
#'
#' @export
useAdjusted <- function(argos, fallback=FALSE, debug=0)
{
    argoFloatsDebug(debug, "useAdjusted(..., fallback=\"", fallback, "\", debug=", debug, ") {\n", sep="", unindent=1, style="bold")
    if (!inherits(argos, "argoFloats"))
        stop("'argos' must be an argoFloats object")
    if ("argos" != argos@metadata$type)
        stop("'argos' must be an argoFloats object created with argoFloats::readProfiles()")
    if (!is.logical(fallback))
        stop("fallback value \"", fallback, "\" is not understood.  It must be TRUE or FALSE")
    res <- argos
    argoList <- argos[["argos"]]
    for (i in seq_along(argoList)) {
        res@data$argos[[i]] <- useAdjustedSingle(argoList[[i]], fallback=fallback, debug=debug-1)
    }
    res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    argoFloatsDebug(debug, "} # useAdjusted()\n", sep="", unindent=1, style="bold")
    res
}

