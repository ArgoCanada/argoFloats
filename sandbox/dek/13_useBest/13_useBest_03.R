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
    if ("dataMode" %in% names(argo@metadata)) { # core
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
    } else if ("parameterDataMode" %in% names(argo@metadata)) { # BGC
        stop("CODE THIS BGC case (likely put bgc into core)")
    }
    res@processingLog <- oce::processingLogAppend(res@processingLog,
                                                  paste0("useAdjustedSingle(argos, fallback=\"", fallback, "\", debug=", debug, ")\n"))
    res
}

#' Switch [[ Focus to Adjusted data
#'
#' This function returns a version of its first argument for which the enclosed
#' [oce::argo-class] objects have been modified in a way that makes
#' future uses of \code{\link{[[,argoFloats-method}}
#' focus (entirely or preferentially) on the *adjusted* data,
#' not the original data.
#'
#' There are two cases.  *Case 1*: If `fallback` is `"NA"`
#' (the default), then the adjusted values are returned, even if they
#' are all `NA`. This is a cautious approach that should yield only
#' data of high quality.  *Case 2*: if `fallback` is `"raw"`, then the data in
#' the adjusted columns are examined one by one. If the values in a given
#' column are all `NA`, then the parameter's raw values are returned.
#' However, if any of the values in a given adjusted column are non-`NA`,
#' then that whole adjusted column is returned, ignoring the raw values.
#' Thus, the `"raw"` method provides a rough estimate even for dubious
#' profiles.
#'
#' @param argo an [`argoFloats-class`] object, as read by [readProfiles()].
#'
#' @param fallback a character value indicating what to do if all the adjusted
#' values for a particular parameter-profile pair are `NA`. The choices are
#' `"NA"` and `"raw"`; see \dQuote{Details}.
#'
#' @author Dan Kelley
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

