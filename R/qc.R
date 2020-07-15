## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Use quality-control flags to alter argoFloats data
#'
#' This function examines the quality-control (QC) flags within an [`argoFloats-class`]
#' object that was created by [readProfiles()].  By default, it replaces all suspicious
#' data with `NA` values, so they will not appear in plots or be considered in calculations.
#' This is an important early step in processing, because suspicious argo floats commonly
#' report data that are suspicious based on statistical and physical measures, as
#' is illustrated in the \dQuote{Examples} section.
#'
#' The work is done by using [oce::handleFlags,argo-method()]
#' on each of the profiles stored within the object. In most cases, only
#' the object needs to be specified, for the default actions coincide with
#' common conventions for flags in argo data.
#'
#' @param x An [`argoFloats-class`] object of type `"argos"`, as created by
#' [readProfiles()].
#'
#' @param flags A vector of integers that are the quality-control flag
#' values for data that are to be set to `NA` in the returned result.
#' The default, `NULL`, means to use the flags set up by [readProfiles()]
#' which, unless specified otherwise in the call to that function, are
#' set up to regard as suspicious any data entries that are flagged
#' with QC codes of 0 (for data that have not yet been assessed),
#' 3 (for "probably bad" data),
#' 4 (for "bad" data),
#' 6 (an unused flag),
#' 7 (an unused flag), or
#' 9 (for "missing" data).  See Section 3.2.2 of Carval et al. (2019) for
#' more information on these QC code values.
#'
#' @param actions The actions to perform. The default, `NULL`, means to
#' use the actions set up by [readProfiles()], which, by default.
#' causes any data flagged as suspicious to be set to `NA`.
#'
#' @param debug Passed to [oce::handleFlags,argo-method()].
#'
#' @return A copy of `x` but with each of the objects within its
#' `data` slot having been passed through [oce::handleFlags,argo-method()].
#'
#' @examples
#' \dontrun{
#' library(argoFloats)
#' # Contrast TS diagrams for raw and flag-handled data
#' data(index)
#' i <- subset(index, 1:5) # first 5 profiles
#' raw <- readProfiles(getProfiles(i))
#' clean <- applyQC(raw)
#' par(mfrow=c(1, 2))
#' plot(raw, which="TS")
#' plot(clean, which="TS")}
#'
#' @references
#' Carval, Thierry, Bob Keeley, Yasushi Takatsuki, Takashi Yoshida, Stephen Loch Loch,
#' Claudia Schmid, and Roger Goldsmith. Argo User’s Manual V3.3. Ifremer, 2019.
#' \url{https://doi.org/10.13155/29825}.
#'
#' @export
#' @author Dan Kelley
applyQC <- function(x, flags=NULL, actions=NULL, debug=0)
{
    if (!requireNamespace("oce", quietly=TRUE))
        stop("must install.packages(\"oce\") for applyQC() to work")
    if ("argos" != x[["type"]])
        stop("can only handle flags in an object of type \"argos\", as created with readProfiles()")
    res <- x
    res@data$argos <- lapply(x@data$argos, oce::handleFlags, flags=flags, actions=actions, debug=debug)
    res@processingLog <- oce::processingLogAppend(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


#' Show QC test results for an argo object
#'
#' `showQCTests` print a summary of the quality-control (QC) tests
#' (if any) that were performed on an argo profile.  It uses
#' [hexToNibble()] to decode the hexadecimal  values that may
#' be stored in `historyQCTest` and `historyAction` within
#' the `metadata` slot of an individual argo profile, as read
#' directly with [oce::read.argo()] or indirectly with
#' [readProfiles()], the latter being illustrated in the
#' \dQuote{Examples} secton below.
#' The format used in `historyQCTest` and `historyAction`
#' is mentioned in Sections 2.2.7, 2.3.7, 5.1, 5.3 and 5.4
#' of Carval et al. (2019), in which they are called
#' `HISTORY_QCTEST` and `HISTORY_ACTION`, respectively.
#'
#' The tests, from Carval et al.(2019) Table 11, are as follows.
#'
#' Test|  Number |    Meaning
#' ----|---------|----------------------
#' 1   |       2 |  Platform Identification test
#' 2   |       4 |  Impossible Date test
#' 3   |       8 |  Impossible Location test
#' 4   |      16 |  Position on Land test
#' 5   |      32 |  Impossible Speed test
#' 6   |      64 |  Global Range test
#' 7   |     128 |  Regional Global Parameter test
#' 8   |     256 |  Pressure Increasing test
#' 9   |     512 |  Spike test
#' 10  |    1024 |  Top and Bottom Spike test (obsolete)
#' 11  |    2048 |  Gradient test
#' 12  |    4096 |  Digit Rollover test
#' 13  |    8192 |  Stuck Value test
#' 14  |   16384 |  Density Inversion test
#' 15  |   32768 |  Grey List test
#' 16  |   65536 |  Gross Salinity or Temperature Sensor Drift test
#' 17  |  131072 |  Visual QC test
#' 18  |  261144 |  Frozen profile test
#' 19  |  524288 |  Deepest pressure test
#' 20  | 1048576 |  Questionable Argos position test
#' 21  | 2097152 |  Near-surface unpumped CTD salinity test
#' 22  | 4194304 |  Near-surface mixed air/water test
#' 23  | 8388608 |  Interim rtqc flag scheme for data deeper than 2000 dbar
#' 24  |16777216 | Interim rtqc flag scheme for data from experimental sensors
#' 25  |33554432 | MEDD test
#'
#' @param x An [oce::argo-class] object, as read directly with [oce::read.argo()]
#' or as extracted from the return value of a call to [readProfiles()], as
#' in the \dQuote{Examples}.
#'
#' @return This function returns nothing; its action is in the printing
#' of results.
#'
#' @examples
#' library(argoFloats)
#' a <- readProfiles(system.file("extdata", "D4900785_048.nc", package="argoFloats"))
#' showQCTests(a[[1]])
#'
#' @references
#' Carval, Thierry, Bob Keeley, Yasushi Takatsuki, Takashi Yoshida, Stephen Loch Loch,
#' Claudia Schmid, and Roger Goldsmith. Argo User’s Manual V3.3. Ifremer, 2019.
#' \url{https://doi.org/10.13155/29825}.
#'
#' @export
#' @author Jaimie Harbin and Dan Kelley
showQCTests <- function(x)
{
    if (!inherits(x, 'argo'))
        stop("can only display Quality Control tests for oce objects of 'argo' class")
    ## Only attempt a display if the object holds HISTORY_ACTION and HISTORY_TESTS
    ## Permit both SNAKE_CASE and camelCase names; oce switched to the latter 2020 Jun 24.
    mnames <- names(x@metadata)
    if ("HISTORY_ACTION" %in% mnames)
        action <- x[['HISTORY_ACTION']]
    else if ("historyAction" %in% mnames)
        action <- x[['historyAction']]
    else
        cat("No Quality Control tests were performed")
        #return(invisible(NULL))
    if ("HISTORY_QCTEST" %in% mnames)
        tests <- x[['HISTORY_QCTEST']]
    else if ("historyQCTest" %in% mnames)
        tests <- x[['historyQCTest']]
    else
        return(invisible(NULL))
    if (is.null(tests))
        return(invisible(NULL))
    ## Match strings within 'action' to find the tests that were performed
    perf <- tests[1, which(action == "QCP$")]
    ## Match strings within 'action' to find the tests that failed
    fail <- tests[1, which(action == "QCF$")]
    ## Break strings into characters
    perfChars <- strsplit(perf, "")[[1]]
    failChars <- strsplit(fail, "")[[1]]
    perfIndices <- which(1 == unlist(lapply(perfChars, function(hex) hexToNibble(hex))))
    failIndices <- which(1 == unlist(lapply(failChars, function(hex) hexToNibble(hex))))
    cat("Tests performed: ", paste(perfIndices, collapse=" "), "\n", sep="")
    cat("Tests failed:    ", paste(failIndices, collapse=" "), "\n", sep="")
    invisible(NULL)
}

