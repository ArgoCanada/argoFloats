## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Switch unadjusted and adjusted data
#'
#' Variables with original names indicating in the string `_ADJUSTED` are
#' assigned nicknams names ending in `Adjusted` by [readProfiles()], so that
#' e.g. `TEMP_ADJUSTED` gets the nickname `temperatureAdjusted`, while
#' `TEMP` gets the nickname `temperature`.  [useAdjusted()] switches
#' these, renaming the adjusted values, so that e.g. `TEMP_ADJUSTED`
#' gets nickname `temperature` and `TEMP` gets nickname `temperatureUnajusted`.
#' This is carried out not just for data, and also for corresponding units and
#' quality-control flags.
#'
#' @param argo An [`argoFloats-class`] object, as read by [readProfiles()].
#'
#' @param debug An integer that, if positive, indicates that some debugging information
#' should be printed.
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
        message("basename '", basename, "'")
        if (length(related) > 1) {
            message("   relatives: '", paste(related, collapse="' '"), "'")
            for (r in related) {
                convert[r] <- if (grepl("Adjusted", r)) gsub("Adjusted", "", r) else paste0(basename, "Unadjusted")
            }
        }
    }
    cat("next is convert:\n")
    print(convert)
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


#' Convert a hex digit to 4 integers indicating the bits, in 'math' order.
#' 
#' This function converts hex digits to 4 integers indicating bits, in 'math' order. It's
#' intended to be used to look at the hex digits of `HISTORY_QCTEST` of an [`argoFloats-class`]
#' object that was created by [readProfiles()]. It then converts these hex digits to bits to
#' be used within the [showQCTests()] function.
#' 
#' @param x a character value corresponding to a hex digit (i.e. 0 through 9, or A through F)
#' 
#' @return an integer vector
#' 
#' @examples
#' \dontrun{
#' hexToNibble('2')
#' hexToNibble('e')
#' }
#' 
#' @author Jaimie Harbina and Dan Kelley
hexToNibble <- function(x)
{
    ## Prepend 0x0 to the character and make it raw, so we can use rawToBits; take
    ## only the rightmost 4 bits since they correspond to our character.
    raw <- as.raw(paste0('0x0', x))
    rawRHS <- tail(rev(rawToBits(as.raw(paste0('0x0', x)))), 4)
    ## Convert to integer, and return the reversed result to compensate for
    ## the rawToBits() ordering.
    ifelse(rawRHS == "01", 1, 0)
}

