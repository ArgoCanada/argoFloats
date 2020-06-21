## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Switch unadjusted and adjusted data
#'
#' Variables with original names indicating in the string `_ADJUSTED` are
#' assigned nicknams names ending in `Adjusted` by [readProfiles()], so that
#' e.g. `DOXY_ADJUSTED` gets the nickname `oxygenAdjusted`, while
#' `DOXY` gets the nickname `oxygen`. `useAdjusted` switches
#' these, renaming the adjusted values, so that e.g. `DOXY_ADJUSTED`
#' gets nickname `oxygen` and `DOXY` gets nickname `oxygenUnajusted`.
#' This is carried out for all data families, and also for the
#' corresponding units and quality-control flags.  See \dQuote{Examples}
#' for an example using the [SD5903586_001.nc] sample dataset.
#'
#' @section Caution regarding realtime and delayed modes:
#'
#' A study of a large set of realtime-mode and delayed-mode datasets
#' suggests that the `_ADJUSTED` fields of the former consist only of `NA`
#' values, so that `useAdjusted` should only be used on delayed-mode
#' datasets. Users are cautioned that `useAdjusted` may
#' changed during the summer of 2020, to detect the mode and perhaps
#' refuse to exchange the data, if doing so would prevent wiping
#' out unadjusted data by replacing them with `NA` values.
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
#' @export
#'
#' @author Dan Kelley
useAdjusted <- function(argo, debug=0)
{
    if (!inherits(argo, "argoFloats"))
        stop("useAdjusted() is only for argoFloats objects")
    if ("argos" != argo[["type"]])
        stop("useAdjusted() is only for argoFloats objects created by readProfiles()")
    res <- argo
    for (i in argo[["length"]])
        res@data$argos[[i]] <- useAdjustedProfile(argo@data$argos[[i]])
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
    namesData <- names(argo@data)
    basenames <- subset(namesData, !grepl("Adjusted", namesData))
    convert <- list()
    for (basename in basenames) {
        w <- grep(basename, namesData)
        related <- namesData[w]
        argoFloatsDebug(debug, "basename '", basename, "'\n", sep="")
        if (length(related) > 1) {
            argoFloatsDebug(debug, "   relatives: '", paste(related, collapse="' '"), "'\n")
            for (r in related) {
                if (grepl("Adjusted$", r)) {
                    convert[r] <- gsub("Adjusted", "", r)
                } else if (grepl("AdjustedError", r)) {
                    convert[r] <- r
                } else {
                    convert[r] <- paste0(basename, "Unadjusted")
                }
            }
        }
    }
    if (debug > 0) {
        cat("next is convert:\n")
        print(convert)
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
    argoFloatsDebug(debug, "} # useAdjusted()\n", style="bold", sep="", unindent=1)
    res
}


#' Convert a hex digit to integer vector of length 4
#'
#' This function converts hex digits to 4 integers indicating bits, in 'math' order. It's
#' intended to be used to look at the hex digits of `HISTORY_QCTEST` of an [`argoFloats-class`]
#' object that was created by [readProfiles()]. It then converts these hex digits to bits to
#' be used within the [showQCTests()] function.
#'
#' @param x A character value corresponding to a hex digit (i.e. 0 through 9, or A through F).
#'
#' @return An integer vector.
#'
#' @examples
#' library(argoFloats)
#' hexToNibble('2')
#' hexToNibble('e')
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

