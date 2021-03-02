rm(list=ls()) # FIXME: helps during development

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
        cat(" DATA_MODE=", paste(argo@metadata$dataMode, collapse=" "), "\n", sep="")
    varNames <- names(argo[["data"]])
    adjustedNames <- grepl("Adjusted$", varNames)
    varNamesRaw <- varNames[!adjustedNames]
    varNamesAdjusted <- varNames[adjustedNames]

    flagNames <- names(argo@metadata$flags)
    adjustedFlagNames <- grepl("Adjusted$", flagNames)
    flagNamesRaw <- flagNames[!adjustedFlagNames]
    flagNamesAdjusted <- flagNames[adjustedFlagNames]
    if (debug > 1) {
        cat("        varNames:          ", paste(varNames, collapse=" "), "\n", sep="")
        cat("        varNamesRaw:       ", paste(varNamesRaw, collapse=" "), "\n", sep="")
        cat("        varNamesAdjusted:  ", paste(varNamesAdjusted, collapse=" "), "\n", sep="")
        cat("        flagNames:         ", paste(flagNames, collapse=" "), "\n", sep="")
        cat("        flagNamesRaw:      ", paste(flagNamesRaw, collapse=" "), "\n", sep="")
        cat("        flagNamesAdjusted: ", paste(flagNamesAdjusted, collapse=" "), "\n", sep="")
    }
    if ("dataMode" %in% names(argo@metadata)) { # core
        dm <- argo@metadata$dataMode[1]
        cat("      non-BGC dataset since dataMode exists\n", sep="")
        for (name in varNamesRaw) {
            adjustedName <- paste0(name, "Adjusted")
            if (adjustedName %in% varNamesAdjusted) {
                nok <- sum(is.finite(argo@data[[adjustedName]]))
                res@data[[name]] <- argo@data[[adjustedName]]
                if (debug > 0)
                    cat("      data: ", adjustedName, " -> ", name, " (", nok, " finite data)\n", sep="")
            }
        }
        for (name in flagNamesRaw) {
            adjustedName <- paste0(name, "Adjusted")
            if (adjustedName %in% flagNamesAdjusted) {
                res@metadata$flags[[name]] <- argo@metadata[[adjustedName]]
                if (debug > 0)
                    cat("      flag: ", adjustedName, " -> ", name, "\n", sep="")
            }
        }
    } else if ("parameterDataMode" %in% names(argo@metadata)) { # BGC
        dm <- argo@metadata$dataMode
        if (debug > 0)
            cat("   BGC dataset (contains \"parameterDataMode\"=\"", paste(dm, collapse="\" \""), "\")\n", sep="")
    }
    res@processingLog <- oce::processingLogAppend(res@processingLog,
                                                  paste0("useAdjustedSingle(argos, fallback=\"", fallback, "\", debug=", debug, ")\n"))
    res
}

#' Switch [[ Focus to Adjusted data
#'
#' FIXME: intro para.
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
#' This function returns a version of its first argument for which the enclosed
#' [oce::argo-class] objects have been modified in a way that makes
#' future uses of \code{\link{[[,argoFloats-method}}
#' focus (entirely or preferentially) on the *adjusted* data,
#' not the original data.
#'
#' @param argo an [`argoFloats-class`] object, as read by [readProfiles()].
#'
#' @param fallback a character value indicating what to do if all the adjusted
#' values for a particular parameter-profile pair are `NA`.
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
    n <- 200
    s <- subset(i, sample(seq_len(i[["length"]]), n))
    a <- readProfiles(getProfiles(s))
}

b <- useAdjustedNEW(a, fallback="NA", debug=1)

