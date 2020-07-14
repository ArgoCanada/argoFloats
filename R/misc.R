## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Switch unadjusted and adjusted data
#'
#' This is done by using [oce::preferAdjusted()] on each of the Argo
#' objects stored within the first argument.
#'
#' This is a complex process, however, simply put, `useAdjusted()` allows users
#' to express preference for adjusted vs unadjusted data. If an Adjusted field exists,
#' and it is not filled with all NA values, this function will return the Adjusted
#' data. If there is no Adjusted data, or it is all NA values, `useAdjusted()` will
#' return the original data.
#'
#' See Reference 1 for the process of adding adjusted data to Argo
#' files, and see the documentation of [oce::preferAdjusted()] for the
#' details of how the preference for adjusted data is set.
#'
#' @param argo An [`argoFloats-class`] object, as read by [readProfiles()].
#'
#' @param debug An integer that, if positive, indicates that some debugging information
#' should be printed.
#'
#' @examples
#' library(argoFloats)
#' # Plot raw and adjusted oxygen for a sample dataset
#' raw <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"))
#' adj <- useAdjusted(raw)
#' par(mfrow=c(1, 2))
#' oce::plotProfile(oce::as.ctd(raw[[1]]), xtype="oxygen")
#' mtext("Raw data", side=3, line=-1, col=2)
#' oce::plotProfile(oce::as.ctd(adj[[1]]), xtype="oxygen")
#' mtext("Adjusted data", side=3, line=-1, col=2)
#'
#' @references
#' Carval, Thierry, Bob Keeley, Yasushi Takatsuki, Takashi Yoshida, Stephen Loch Loch,
#' Claudia Schmid, and Roger Goldsmith. Argo User’s Manual V3.3. Ifremer, 2019.
#' \url{https://doi.org/10.13155/29825}.
#'
#' @export
#'
#' @author Dan Kelley and Jaimie Harbin
useAdjusted <- function(argo, debug=0)
{
    if (!inherits(argo, "argoFloats"))
        stop("useAdjusted() is only for argoFloats objects")
    if ("argos" != argo[["type"]])
        stop("useAdjusted() is only for argoFloats objects created by readProfiles()")
    res <- argo
    for (i in argo[["length"]])
        res@data$argos[[i]] <- oce::preferAdjusted(argo@data$argos[[i]])
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
#' `hexToNibble` converts a hexadecimal digit to 4 integers indicating bits, e.g. for use within
#' [showQCTests()].
#'
#' @param x A character value corresponding to a hexadecimal digit (i.e. `"0"` through `"9"`,
#' `"a"` through `"f"`, or `"A"` through `"F"`).
#'
#' @return An integer vector, in 'mathematical' order.  (This is the reverse of
#' the order used by [rawToBits()]; see the \dQuote{Examples}.)
#'
#' @examples
#' library(argoFloats)
#' hexToNibble('1') # 0 0 0 1
#' hexToNibble('e') # 1 1 1 0
#'
#' @export
#'
#' @author Jaimie Harbin and Dan Kelley
hexToNibble <- function(x)
{
    ## Prepend 0x0 to the character and make it raw, so we can use rawToBits; take
    ## only the rightmost 4 bits since they correspond to our character.
    raw <- as.raw(paste0('0x0', x))
    rawRHS <- tail(rev(rawToBits(as.raw(paste0('0x0', x)))), 4)
    ifelse(rawRHS == "01", 1, 0)
}

