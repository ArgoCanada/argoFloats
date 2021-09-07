## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4:tw=120

# UNEXPORTED support function for useAdjusted() on single oce::argo object
useAdjustedSingle <- function(argo, fallback=FALSE, debug=0)
{
    if (!(inherits(argo, "oce") && inherits(argo, "argo")))
        stop("First argument must be an oce::argo object")
    argoFloatsDebug(debug, "useAdjustedSingle(..., fallback=", fallback, ", debug=", debug, ") {\n", sep="", unindent=1, style="bold")
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
    # argoFloatsDebug(debug, "pressure is a ", ncol, "x", nrow, " matrix\n", sep="")
    stationParameters <- argo@metadata$stationParameters
    if (debug > 1) {
        argoFloatsDebug(debug, "next is stationParameters\n")
        print(stationParameters)
    }
    isBGC <- "parameterDataMode" %in% names(argo@metadata)
    if (!isBGC) {
        argoFloatsDebug(debug, "non-BGC file\n")
        dataMode <- argo@metadata$dataMode
        if (any(!dataMode %in% c("R", "A", "D"))) {
            warning("skipping a cycle, because some dataMode values are not \"R\", \"A\" or \"D\"\n")
            return(argo)
        }
        # Set up dnoRev, to find argoFloats name given NetCDF name
        dno <- argo@metadata$dataNamesOriginal
        dnoRev <- list()
        for (name in names(dno))
            dnoRev[[dno[[name]]]] <- name
        if (debug > 1) {               # show only at a high debug level
            cat("dnoRev follows:\n")
            cat(str(dnoRev))
        }
        #varNamesRaw <- unlist(lapply(parameters, function(n) dnoRev[[n]]))
        for (icol in seq_len(ncol)) { # non-BGC case
            mode <- dataMode[icol]
            # Dewey Dunnington realized that 'stationParameters' is what we need next, not 'parameters'.
            # (See https://github.com/ArgoCanada/argoFloats/issues/418 for discussion.)
            parameters <- trimws(as.vector(argo@metadata$stationParameters[,icol]))
            parameters <- parameters[nchar(parameters) > 0]
            varNamesRaw <- unlist(lapply(parameters, function(n) dnoRev[[n]]))
            argoFloatsDebug(debug, "Profile ", icol, " of ", ncol, "\n", sep="")
            argoFloatsDebug(debug, "  mode:       \"", mode, "\"\n", sep="")
            argoFloatsDebug(debug, "  parameters: ", paste(parameters,collapse=" "), "\n", sep="")
            # cat("next is varNamesAdjusted\n");print(varNamesAdjusted)
            for (name in varNamesRaw) {
                argoFloatsDebug(debug, "    ", name, " (AKA ", dno[[name]], ")\n", sep="")
                #argoFloatsDebug(debug, "name: \"", name, "\"", sep="")
                adjustedName <- paste0(name, "Adjusted")
                #argoFloatsDebug(debug, "name=\"", name, "\", adjustedName=\"", adjustedName, "\"\n", sep="")
                # Can only copy if we have an Adjusted field (which is not always the case).
                if (adjustedName %in% varNamesAdjusted) {
                    # Copy <PARAM>Adjusted into <PARAM> if either of the following is true.
                    #    Case 1. fallback is FALSE
                    #    Case 2. fallback is TRUE and mode is "A" or "D"
                    case <- if (fallback == FALSE) { 1 } else if (mode %in% c("A", "D")) { 2 } else { 0 }
                    if (case > 0) {
                        res@data[[name]][,icol] <- argo@data[[adjustedName]][,icol]
                        res@metadata$flags[[name]][,icol] <- argo@metadata$flags[[adjustedName]][,icol]
                        if (!fallback) {
                            argoFloatsDebug(debug, "      copied ", adjustedName, " to ", name, ", since fallback=", fallback, "\n", sep="")
                        } else {
                            argoFloatsDebug(debug, "      copied ", adjustedName, " to ", name, ", since fallback=", fallback, " and mode=", mode, "\n", sep="")
                        }
                    } else {
                        if (!fallback) {
                            argoFloatsDebug(debug, "      retaining original, since fallback=", fallback, "\n", sep="")
                        } else {
                            argoFloatsDebug(debug, "      retaining original, since fallback=", fallback, " and data-mode is ", pdmThis, "\n", sep="")
                        }
                    }
                } else {
                    argoFloatsDebug(debug, "      retaining original, since ", adjustedName, " is not present\n", sep="")
                }
            }
        }
    } else {
        argoFloatsDebug(debug, "BGC (or synthetic) file\n")
        # dnoRev is the reverse of dataNamesOriginal, and is for looking up
        # items within parameterDataMode as we work through the columns (i.e.
        # profiles).
        dno <- argo@metadata$dataNamesOriginal
        dnoRev <- list()
        for (name in names(dno))
            dnoRev[[dno[[name]]]] <- name
        if (debug > 1) {               # show only at a high debug level
            cat("dnoRev follows:\n")
            cat(str(dnoRev))
        }
        for (icol in seq_len(ncol)) { # BGC case
            pdm <- argo@metadata$parameterDataMode[icol]
            # Dewey Dunnington realized that 'stationParameters' is what we need next, not 'parameters'.
            # (See https://github.com/ArgoCanada/argoFloats/issues/418 for discussion.)
            parameters <- trimws(as.vector(argo@metadata$stationParameters[,icol]))
            parameters <- parameters[nchar(parameters) > 0]
            argoFloatsDebug(debug, "Profile ", icol, " of ", ncol, "\n", sep="")
            argoFloatsDebug(debug, "  mode:       \"", pdm, "\"\n", sep="")
            argoFloatsDebug(debug, "  parameters: ", paste(parameters,collapse=" "), "\n", sep="")
            varNamesRaw <- unlist(lapply(parameters, function(n) dnoRev[[n]]))
            for (name in varNamesRaw) {
                argoFloatsDebug(debug, "    ", name, " (AKA ", dno[[name]], ")\n", sep="")
                adjustedName <- paste0(name, "Adjusted")
                # Can only copy if we have an Adjusted field (which is not always the case).
                if (adjustedName %in% varNamesAdjusted) {
                    # Look up data-mode for this variable in this profile
                    w <- which(parameters == dno[name])
                    if (length(w)) {
                        pdmThis <- substr(pdm, w, w)
                        # Copy <PARAM>Adjusted into <PARAM> if either of the following is true.
                        #    Case 1. fallback is FALSE
                        #    Case 2. fallback is TRUE and mode is "A" or "D"
                        case <- if (!fallback) { 1 } else if (pdmThis %in% c("A", "D")) { 2 } else { 0 }
                        if (case > 0) {
                            res@data[[name]][,icol] <- argo@data[[adjustedName]][,icol]
                            res@metadata$flags[[name]][,icol] <- argo@metadata$flags[[adjustedName]][,icol]
                            if (!fallback) {
                                argoFloatsDebug(debug, "      copied ", adjustedName, " to ", name, ", since fallback=", fallback, "\n", sep="")
                            } else {
                                argoFloatsDebug(debug, "      copied ", adjustedName, " to ", name, ", since fallback=", fallback, " and mode=", pdmThis, "\n", sep="")
                            }
                        } else {
                            if (!fallback) {
                                argoFloatsDebug(debug, "      retaining original, since fallback=", fallback, "\n", sep="")
                            } else {
                                argoFloatsDebug(debug, "      retaining original, since fallback=", fallback, " and data-mode is ", pdmThis, "\n", sep="")
                            }
                        }
                    } else {
                        argoFloatsDebug(debug, "      \"", name, "\" is not present in this profile\n")
                    }
                } else {
                    argoFloatsDebug(debug, "      retaining original, since ", adjustedName, " is not present\n", sep="")
                }
            }
            # argoFloatsDebug(debug, "this cycle is of not of 'core' type; next is parameterDataMode:\n")
            # if (debug)
            #     print(parameterDataMode)
        }
    }
    res@processingLog <- oce::processingLogAppend(res@processingLog,
                                                  paste0("useAdjustedSingle(argo, fallback=\"", fallback, "\", debug=", debug, ")\n"))
    argoFloatsDebug(debug, "} # useAdjustedSingle()\n", sep="", unindent=1, style="bold")
    res
}

#' Switch [[ and Plot to Focus on Adjusted Data, if Available
#'
#' `useAdjusted` returns a copy of an [`argoFloats-class`] object
#' within which the individual
#' [oce::argo-class] objects may have been modified so that future calls to
#' \code{\link{[[,argoFloats-method}}
#' or \code{\link{plot,argoFloats-method}}
#' will focus with
#' *adjusted* versions of the data. (Note that this modification cannot
#' be done for fields that lack adjusted values, so in such cases future
#' calls to
#' \code{\link{[[,argoFloats-method}}
#' or
#' \code{\link{plot,argoFloats-method}}
#' work with the unadjusted fields.)
#' The procedure is done profile by profile and parameter by parameter.
#' The `fallback` argument offers a way to ''fall back'' to unadjusted
#' values, depending on the data-mode (real-time, adjusted or delayed)
#' for individual items; see \dQuote{Details}.
#'
#' There are two cases.  First, if `fallback` is `FALSE` (which the default)
#' then the focus switches entirely
#' to the adjusted data.  This improves the overall reliability of the results,
#' but at the cost of eliminating real-time data.  This is because the
#' adjusted fields for real-time data are set to `NA` as a matter of policy (see
#' section 2.2.5 of reference 1).
#'
#' Wider data coverage is obtained  if `fallback` is `TRUE`.  In this
#' case, the focus is shifted to adjusted data *only if* the data-mode for
#' the individual profiles is `A` or `D`, indicating either Adjusted or
#' Delayed mode. Any profiles that are of the `R` (Realtime) data-mode are
#' left unaltered. This blending of adjusted and unadjusted data offers
#' improved spatial and temporal coverage, while reducing the overall
#' data quality, and so this approach should be used with caution. For more
#' on this function see section 3.4 of Kelley et. al (2021).
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
#' raw <- readProfiles(system.file("extdata", file, package="argoFloats"))
#' adj <- useAdjusted(raw)
#' # Autoscale with adjusted values so frame shows both raw and adjusted.
#' plot(adj, which="profile", profileControl=list(parameter="oxygen"), pch=2)
#' points(raw[[1]][["oxygen"]], raw[[1]][["pressure"]], pch=1)
#' legend("bottomright", pch=c(2,1), legend=c("Raw", "Adjusted"))
#'
#' @author Dan Kelley, Jaimie Harbin and Clark Richards
#'
#' @references
#' 1. Argo Data Management Team. "Argo User's Manual V3.4,"
#' January 20, 2021. `https://archimer.ifremer.fr/doc/00187/29825/`
#'
#' 2. Kelley, D. E., Harbin, J., & Richards, C. (2021). argoFloats: An R package for analyzing
#' Argo data. Frontiers in Marine Science, (8), 636922.
#' \doi{10.3389/fmars.2021.635922}
#'
#' @return `useAdjusted` returns an [`argoFloats-class`] object that is similar to
#' its first argument, but which is set up so that future calls to
#' \code{\link{[[,argoFloats-method}} or \code{\link{plot,argoFloats-method}}
#' will focus on the "adjusted" data stream.
#'
#' @importFrom utils str
#' @export
useAdjusted <- function(argos, fallback=FALSE, debug=0)
{
    argoFloatsDebug(debug, "useAdjusted(..., fallback=", fallback, ", debug=", debug, ") {\n", sep="", unindent=1, style="bold")
    if (!inherits(argos, "argoFloats"))
        stop("'argos' must be an argoFloats object")
    if ("argos" != argos@metadata$type)
        stop("'argos' must be an argoFloats object created with argoFloats::readProfiles()")
    if (!is.logical(fallback))
        stop("fallback value \"", fallback, "\" is not understood.  It must be TRUE or FALSE")
    debug <- ifelse(debug > 3, 3L, as.integer(debug)) # limit depth
    res <- argos
    argoList <- argos[["argos"]]
    for (i in seq_along(argoList)) {
        res@data$argos[[i]] <- useAdjustedSingle(argoList[[i]], fallback=fallback, debug=debug-1)
    }
    res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    argoFloatsDebug(debug, "} # useAdjusted()\n", sep="", unindent=1, style="bold")
    res
}

