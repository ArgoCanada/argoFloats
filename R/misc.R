## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Switch unadjusted and adjusted data
#'
#' This is done by using [oce::preferAdjusted()] on each of the Argo
#' objects stored within the first argument.  The procedure is fairly
#' complicated, so users are urged to read the documentation
#' for [oce::preferAdjusted()] after reading the
#' sketch provided in the \dQuote{Details} section and to
#' familiarizing themselves with the flow chart below that demonstrates the
#' steps taken when a user specifies `which=ALL` or `which=<param>`.
#'
#' The value returned by `useAdjusted` is similar to the first argument
#' in all respects, except that the individual `argo` objects stored within
#' its `data` slot have their `metadata` slots modified to indicate a
#' preference for adjusted data over unadjusted data. Because of that,
#' future calls to \code{\link{[[,argoFloats-method}} will
#' return adjusted data instead of unadjusted data, for data items
#' specified by `which`, and subject to the constraint imposed by
#' `fallback`.
#'
#' See the documentation for [oce::preferAdjusted()] for more
#' on the procedure, and see Reference 1 for the process of adding
#' adjusted data to Argo files.  Although it might seem that adjusted data
#' will always be preferable to unadjusted data, this is not always
#' the case, and a careful analyst will study both data streams, and
#' look through the processing notes for any float that is of individual
#' interest.
#'
#' \if{html}{\figure{useAdjustedDiagram.png}{options: width=455px alt="Figure: useAdjustedDiagram.png" fig.cap="hello"}}
#'
#' @param argo an [`argoFloats-class`] object, as read by [readProfiles()].
#'
#' @param which a character vector (passed to
#' [oce::preferAdjusted()] directly) naming the items for which
#' (depending also on the value of `fallback`) adjusted values
#' are to be sought by future calls to \code{\link{[[,argoFloats-method}}.
#' The short names are used, e.g. `which="oxygen"` means that
#' adjusted oxygen is to be returned in future calls
#' such as `argo[["oxygen"]]`.  The default,
#' `"all"`, means to  use adjusted values for any item in `argo`
#' that has adjusted values.
#'
#' @param fallback a logical value (passed to
#' [oce::preferAdjusted()] directly) indicating whether to fall back
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
#' raw <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"))
#' adj <- useAdjusted(raw)
#' par(mfrow=c(1,2), mar=c(5,4,1,2))
#' hist(raw[[1]][['oxygen']], xlab='Raw Oxygen', ylab="Frequency", main=NULL)
#' hist(adj[[1]][['oxygen']], xlab='Adjusted Oxygen', ylab="Frequency", main=NULL)
#'
#' @references
#' 1. Carval, Thierry, Bob Keeley, Yasushi Takatsuki, Takashi Yoshida, Stephen Loch Loch,
#' Claudia Schmid, and Roger Goldsmith. Argo User’s Manual V3.3. Ifremer, 2019.
#' \url{https://doi.org/10.13155/29825}.
#'
#' @export
#'
#' @author Dan Kelley and Jaimie Harbin
useAdjusted <- function(argo, which="all", fallback=TRUE, debug=0)
{
    if (!inherits(argo, "argoFloats"))
        stop("useAdjusted() is only for argoFloats objects")
    if ("argos" != argo[["type"]])
        stop("useAdjusted() is only for argoFloats objects created by readProfiles()")
    res <- argo
    for (i in argo[["length"]])
        res@data$argos[[i]] <- oce::preferAdjusted(argo@data$argos[[i]], which=which, fallback=fallback)
    res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

useAdjustedProfile <- function(argo, debug=0)
{
    debug <- max(c(0, floor(debug)))
    if (!requireNamespace("oce", quietly=TRUE))
        stop("must install.packages(\"oce\") for useAdjusted() to work")
    argoFloatsDebug(debug, "useAdjusted() {\n", style="bold", sep="", unindent=1)
    res <- argo
    ## Step 1. Find names of related variables, and set up 'convert', which is a
    ## key to renaming things.  For example, "oxygenAdjusted" in the original will
    ## become "oxygen", and "oxygen" in the original will become "oxygenUnadjusted".
    namesData <- names(argo@data)
    basenames <- subset(namesData, !grepl("Adjusted", namesData))
    convert <- list()
    for (basename in basenames) {
        w <- grep(basename, namesData) ## FIXME: what if e.g. "oxygen" and "oxygenFrequency" co-occur in a profile?
        related <- namesData[w]
        argoFloatsDebug(debug, "basename '", basename, "'\n", sep="")
        if (length(related) > 1) {
            argoFloatsDebug(debug, "   relatives: '", paste(related, collapse="' '"), "'\n")
            for (r in related) {
                ## FIXME: only do this if the Adjusted field has non-NA data.
                ##??? if (any(is.finite(x@data[[basename]]))) {
                if (grepl("Adjusted$", r)) {
                    convert[r] <- gsub("Adjusted", "", r)
                } else if (grepl("AdjustedError", r)) {
                    convert[r] <- r
                } else {
                    convert[r] <- paste0(basename, "Unadjusted")
                }
                ##??? }
            }
        }
    }
    if (debug > 0) {
        cat("next is convert:\n")
        print(convert)
    }
    namesConvert <- names(convert)
    ## Step 2. Rename data
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
    ## Step 3. Rename metadata$flags
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
    ## Step 4. Rename metadata$units
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
    ## Step 5. Rename metadata$dataNamesOriginal
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
    argoFloatsDebug(debug, "} # useAdjusted()\n", style="bold", sep="", unindent=1)
    res
}


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

