library(argoFloats)

useAdjustedSingle <- function(argo, fallback="NA", debug=0)
{
    if (!(inherits(argo, "oce") && inherits(argo, "argo")))
        stop("First argument must be an oce::argo object")
    res <- argo
    fn <- argo[["filename"]]
    typeFromFilename <- switch(substring(gsub(".*/","",fn),1,1), "A"="adjusted", "D"="delayed", "R"="realtime")
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

#' Switch [[ and Plot Focus to Adjusted data
#'
#' This function returns a copy of its first argument that has been modified by
#' (optionally) replacing 'raw' parameter values, and their flags, with
#' corresponding 'adjusted' values. The purpose is to ensure that future calls
#' to \code{\link{[[,argoFloats-method}} and plotting methods will focus on
#' the adjusted data instead of the raw data.  The procedure hinges on the value
#' of the `fallback` argument, as explained in \dQuote{Details}.
#'
#' There are two cases.  *Case 1*: If `fallback` is `"NA"` (the default), then
#' the adjusted values become the focus, even if they are all `NA`. Thus,
#' `fallback="NA"` may be preferred as part of a cautious analysis that
#' focuses only on data of high quality. The downside of this approach is a
#' reduction in coverage, since it eliminates the 'raw' fields that are typical
#' of real-time mode datasets.
#'
#' *Case 2*: if `fallback` is `"raw"`, then the data in the adjusted parameter
#' data columns (corresponding to profiles) of each cycle are examined one by
#' one. If the values in a given column are all `NA`, then the parameter's raw
#' values for that column are not altered. However, if any of the values in a
#' given adjusted column are non-`NA`, then that entire 'adjusted' column is
#' copied into the corresponding 'raw' column. Since the `fallback="raw"`
#' scheme combines 'raw' and 'adjusted' data, it increases coverage, at the
#' possible expense of lowering overall data quality.
#'
#' @param argo an [`argoFloats-class`] object, as read by [readProfiles()].
#'
#' @param fallback a character value indicating what to do if all the adjusted
#' values for a particular parameter-profile pair are `NA`. The choices are
#' `"NA"` and `"raw"`; see \dQuote{Details}.
#'
#' @author Dan Kelley and Jaimie Harbin
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

set.seed(408)
if (!exists("a")) {
    i <- getIndex(age=10) # no need to get latest and greatest
    #n <- 2
    n <- 200
    s <- subset(i, sample(seq_len(i[["length"]]), n))
    a <- readProfiles(getProfiles(s))
}

cat("###### NA case ##########\n")
B <- useAdjustedNEW(a, fallback="NA", debug=1)
cat("###### raw case ##########\n")
C <- useAdjustedNEW(a, fallback="raw", debug=1)

