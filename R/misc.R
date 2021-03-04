## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Switch [[ Focus to Adjusted data
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
#' @export
#'
#' @author Dan Kelley and Jaimie Harbin
useAdjusted <- function(argo, which="all", fallback=TRUE, debug=0)
{
    argoFloatsDebug(debug, "useAdjusted() {\n", sep="", unindent=1, style="bold")
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
    argoFloatsDebug(debug, "} # \n", sep="", unindent=1, style="bold")
    res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

useAdjustedSingle <- function(argo, fallback="NA", debug=0)
{
    if (!(inherits(argo, "oce") && inherits(argo, "argo")))
        stop("First argument must be an oce::argo object")
    res <- argo
    fn <- argo[["filename"]]
    typeFromFilename <- switch(substring(gsub(".*/","",fn),1,1), "A"="adjusted", "D"="delayed", "R"="realtime")
    if (debug > 0)
        cat(sprintf("%s (%s?)", gsub(".*/", "", fn), typeFromFilename))
    if (debug > 0 && "dataMode" %in% names(argo@metadata))
        cat(" fallback=", fallback, ", DATA_MODE=", paste(argo@metadata$dataMode, collapse=" "), "\n", sep="")
    varNames <- names(argo[["data"]])
    adjustedNames <- grepl("Adjusted$", varNames)
    varNamesRaw <- varNames[!adjustedNames]
    varNamesAdjusted <- varNames[adjustedNames]
    if (debug > 1) {
        cat("        varNames:          ", paste(varNames, collapse=" "), "\n", sep="")
        cat("        varNamesRaw:       ", paste(varNamesRaw, collapse=" "), "\n", sep="")
        cat("        varNamesAdjusted:  ", paste(varNamesAdjusted, collapse=" "), "\n", sep="")
    }
    #>>> if ("dataMode" %in% names(argo@metadata)) { # core
    if (debug > 0)
        cat("      non-BGC dataset since dataMode exists\n", sep="")
    nrow <- dim(argo@data$pressure)[1]
    ncol <- dim(argo@data$pressure)[2]
    for (name in varNamesRaw) {
        adjustedName <- paste0(name, "Adjusted")
        # There should always be an Adjusted field, but we check to be safe.
        if (adjustedName %in% varNamesAdjusted) {
            # We use the Adjusted field if fallback=="NA", or if
            # fallback=="raw" and some adjusted data are non-NA.
            for (icol in seq_len(ncol)) {
                nok <- sum(is.finite(argo@data[[adjustedName]][,icol]))
                if (debug > 0)
                    cat("      name=", name, ", icol=", icol, ", nok=", nok, "\n", sep="")
                if (fallback == "NA" || nok > 0) {
                    res@data[[name]][,icol] <- argo@data[[adjustedName]][,icol]
                    res@metadata$flags[[name]][,icol] <- argo@metadata$flags[[adjustedName]][,icol]
                    if (debug > 0)
                        cat("      ", adjustedName, "[,", icol, "] -> ",
                            name, "[,", icol, "] (# non-NA Adjusted values: ",
                            nok, " or ", round(100*nok/nrow, 2), "%)\n", sep="")
                }
            }
        }
    }
    #>>> } else if ("parameterDataMode" %in% names(argo@metadata)) { # BGC
    #>>>     stop("CODE THIS BGC case (likely put bgc into core)")
    #>>> }
    res@processingLog <- oce::processingLogAppend(res@processingLog,
                                                  paste0("useAdjustedSingle(argos, fallback=\"", fallback, "\", debug=", debug, ")\n"))
    res
}

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
#' @param debug an integer that, if positive, indicates that some debugging information
#' should be printed.
#'
#' @author Dan Kelley and Jaimie Harbin
#'
#' @references
#' 1. Argo Data Management Team. "Argo User’s Manual V3.4,"
#' January 20, 2021. <https://archimer.ifremer.fr/doc/00187/29825/>
#'
#' @export
useAdjustedNEW <- function(argos, fallback="NA", debug=0)
{
    if (!inherits(argos, "argoFloats"))
        stop("'argos' must be an argoFloats object")
    if ("argos" != argos@metadata$type)
        stop("'argos' must be an argoFloats object created with argoFloats::readProfiles()")
    if (!fallback %in% c("NA", "raw"))
        stop("fallback value \"", fallback, "\" is not understood.  It must be \"NA\" or \"raw\"")
    res <- argos
    argoList <- argos[["argos"]]
    for (i in seq_along(argoList)) {
        if (debug > 0)
            cat(sprintf("\n%2d. ", i))
        res@data$argos[[i]] <- useAdjustedSingle(argoList[[i]], fallback=fallback, debug=debug)
    }
    res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


#useAdjustedProfile <- function(argo, debug=0)
#{
#    debug <- max(c(0, floor(debug)))
#    if (!requireNamespace("oce", quietly=TRUE))
#        stop("must install.packages(\"oce\") for useAdjusted() to work")
#    argoFloatsDebug(debug, "useAdjusted() {\n", style="bold", sep="", unindent=1)
#    res <- argo
#    ## Step 1. Find names of related variables, and set up 'convert', which is a
#    ## key to renaming things.  For example, "oxygenAdjusted" in the original will
#    ## become "oxygen", and "oxygen" in the original will become "oxygenUnadjusted".
#    namesData <- names(argo@data)
#    basenames <- subset(namesData, !grepl("Adjusted", namesData))
#    convert <- list()
#    for (basename in basenames) {
#        w <- grep(basename, namesData) ## FIXME: what if e.g. "oxygen" and "oxygenFrequency" co-occur in a profile?
#        related <- namesData[w]
#        argoFloatsDebug(debug, "basename '", basename, "'\n", sep="")
#        if (length(related) > 1) {
#            argoFloatsDebug(debug, "   relatives: '", paste(related, collapse="' '"), "'\n")
#            for (r in related) {
#                ## FIXME: only do this if the Adjusted field has non-NA data.
#                ##??? if (any(is.finite(x@data[[basename]]))) {
#                if (grepl("Adjusted$", r)) {
#                    convert[r] <- gsub("Adjusted", "", r)
#                } else if (grepl("AdjustedError", r)) {
#                    convert[r] <- r
#                } else {
#                    convert[r] <- paste0(basename, "Unadjusted")
#                }
#                ##??? }
#            }
#        }
#    }
#    ##> if (debug > 0) {
#    ##>     cat("next is convert:\n")
#    ##>     print(convert)
#    ##> }
#    namesConvert <- names(convert)
#    ## Step 2. Rename data
#    namesData <- names(argo@data)
#    tmp <- namesData
#    for (i in seq_along(tmp)) {
#        w <- which(namesData[i] == namesConvert)
#        if (length(w))
#            tmp[i] <- convert[[w]]
#    }
#    argoFloatsDebug(debug, "data ORIG:  ", paste(names(argo@data), collapse=" "), "\n")
#    names(res@data) <- tmp
#    argoFloatsDebug(debug, "data AFTER: ", paste(names(res@data), collapse=" "), "\n")
#    ## Step 3. Rename metadata$flags
#    namesFlags <- names(argo@metadata$flags)
#    tmp <- namesFlags
#    for (i in seq_along(tmp)) {
#        w <- which(namesFlags[i] == namesConvert)
#        if (length(w))
#            tmp[i] <- convert[[w]]
#    }
#    argoFloatsDebug(debug, "flags ORIG:  ", paste(names(argo@metadata$flags), collapse=" "), "\n")
#    names(res@metadata$flags) <- tmp
#    argoFloatsDebug(debug, "flags AFTER: ", paste(names(res@metadata$flags), collapse=" "), "\n")
#    ## Step 4. Rename metadata$units
#    namesUnits <- names(argo@metadata$units)
#    tmp <- namesUnits
#    for (i in seq_along(tmp)) {
#        w <- which(namesUnits[i] == namesConvert)
#        if (length(w))
#            tmp[i] <- convert[[w]]
#    }
#    argoFloatsDebug(debug, "units ORIG:  ", paste(names(argo@metadata$units), collapse=" "), "\n")
#    names(res@metadata$units) <- tmp
#    argoFloatsDebug(debug, "units AFTER: ", paste(names(res@metadata$units), collapse=" "), "\n")
#    ## Step 5. Rename metadata$dataNamesOriginal
#    namesUnits <- names(argo@metadata$dataNamesOriginal)
#    tmp <- namesUnits
#    for (i in seq_along(tmp)) {
#        w <- which(namesUnits[i] == namesConvert)
#        if (length(w))
#            tmp[i] <- convert[[w]]
#    }
#    argoFloatsDebug(debug, "dataNamesOriginal ORIG:  ", paste(names(argo@metadata$dataNamesOriginal), collapse=" "), "\n")
#    names(res@metadata$dataNamesOriginal) <- tmp
#    argoFloatsDebug(debug, "dataNamesOriginal AFTER: ", paste(names(res@metadata$dataNamesOriginal), collapse=" "), "\n")
#    res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
#    argoFloatsDebug(debug, "} # useAdjusted()\n", style="bold", sep="", unindent=1)
#    res
#}


#' Convert Hexadecimal Digit to Integer Vector
#'
#' `hexToBits` converts a string holding hexadecimal digits to a sequence of integers
#' 0 or 1, for the bits.  This is mainly for for use within [showQCTests()].
#'
#' @param hex a vector of character values corresponding to a sequence of one or more
#' hexadecimal digits (i.e. `"0"` through `"9"`,`"a"` through `"f"`, or `"A"` through `"F"`).
#'
#' @return An integer vector holding the bits as values 0 or 1.  The
#' inverse of 'mathematical' order is used, as is the case for the base
#' R function [rawToBits()]; see the \dQuote{Examples}.
#'
#' @examples
#' library(argoFloats)
#' hexToBits('3')    # 1 1 0 0
#' hexToBits('4000') # 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
#'
#' @export
#'
#' @author Jaimie Harbin and Dan Kelley
hexToBits <- function(hex)
{
    ## see https://github.com/ArgoCanada/argoFloats/issues/176
    res <- NULL
    for (h in strsplit(hex, "")[[1]]) {
        bits <- ifelse(01 == rawToBits(as.raw(paste0("0x0", h))), 1, 0)
        res <- c(res, tail(rev(bits), 4))
    }
    rev(res)
}

