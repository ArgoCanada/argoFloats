## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4:tw=80

# UNEXPORTED support function for useAdjusted() on single oce::argo object
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
        # dnoRev is the reverse of dataNamesOriginal, and is for looking up
        # items within parameterDataMode as we work through the columns (i.e.
        # profiles).
        dno <- argo@metadata$dataNamesOriginal
        dnoRev <- list()
        for (name in names(dno))
            dnoRev[[dno[[name]]]] <- name
        if (debug > 1) {               # show only at a high debug level
            cat("dnoRev follows:\n")
            print(dnoRev)
        }
        for (icol in seq_len(ncol)) {
            pdm <- argo@metadata$parameterDataMode[icol]
            if (debug > 1) {           # show only at a high debug level
                cat("pdm follows:\n")
                print(pdm)
            }
            for (name in varNamesRaw) {
                adjustedName <- paste0(name, "Adjusted")
                # There should always be an Adjusted field, but we check to be safe.
                if (adjustedName %in% varNamesAdjusted) {
                    cat("handle ", adjustedName, "->", name, "here\n")
                    print(dno[name])
                    parameters <- argo@metadata$parameter[,,icol]
                    if (debug > 0) {
                        cat("parameters follows:\n")
                        print(parameters)
                    }
                    #print(dnoRev[name])
                }
            }
            stop()
            argoFloatsDebug(debug, "this cycle is of not of 'core' type; next is parameterDataMode:\n")
            if (debug)
                print(parameterDataMode)
        }
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
#' observed data to adjusted data.  This works profile-by-profile, for each of
#' the [oce::argo-class] objects stored within the first argument.
#' The details of how this works are controlled by
#' the `fallback` argument.
#'
#' If `fallback` is `FALSE`, which the default, then the focus switches entirely
#' to the adjusted data.  This improves the overall reliability of the results,
#' but at the cost of eliminating realtime-mode data.  This is because the
#' adjusted fields for realtime data are set to `NA` as a matter of policy (see
#' section 2.2.5 of reference 1).
#'
#' Wider data coverage is obtained if `fallback` is set to `TRUE`.  In this
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

