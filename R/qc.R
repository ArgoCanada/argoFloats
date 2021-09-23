## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Apply Quality Control Flags
#'
#' This function examines the quality-control ("QC") flags within an [`argoFloats-class`]
#' object that was created by [readProfiles()].  By default, it replaces all suspicious
#' data with `NA` values, so they will not appear in plots or be considered in calculations.
#' This is an important early step in processing, because suspicious Argo floats commonly
#' report data that are suspicious based on statistical and physical measures, as
#' is illustrated in the \dQuote{Examples} section. See section 3.3 of Kelley et al. (2021) for more on this function.
#'
#' The work is done by using [oce::handleFlags,argo-method()]
#' on each of the profiles stored within the object. In most cases, only
#' the object needs to be specified, for the default actions coincide with
#' common conventions for flags in Argo data.
#'
#' @param x an [`argoFloats-class`] object of type `"argos"`, as created by
#' [readProfiles()].
#'
#' @param flags A list specifying flag values upon which actions will be taken. This can take two forms.
#'
#' In the first form, the list has named elements each containing a vector of integers. For example, salinities flagged with values of 1 or 3:9 would be specified by flags=list(salinity=c(1,3:9)). Several data items can be specified, e.g. flags=list(salinity=c(1,3:9), temperature=c(1,3:9)) indicates that the actions are to take place for both salinity and temperature.
#'
#' In the second form, flags is a list holding a single unnamed vector, and this means to apply the actions to all the data entries. For example, flags=list(c(1,3:9)) means to apply not just to salinity and temperature, but to everything within the data slot.
#'
#' If flags is NULL then flags=list(c(0,3,4,6,7,9)) is used by default where:
#' 0 = data that have not yet been assessed,
#' 3 = "probably bad" data,
#' 4 = for "bad" data,
#' 6 = an unused flag,
#' 7 = an unused flag, or
#' 9 = "missing" data.
#'
#'See Sections 3.2.1 and 3.2.2 of Carval et al. (2019) for
#' more information on these QC code values.
#'
#' @param actions the actions to perform. The default, `NULL`, means to
#' use the actions set up by [readProfiles()], which, by default.
#' causes any data flagged as suspicious to be set to `NA`.
#'
#' @param debug an integer passed to [oce::handleFlags,argo-method()].  If this
#' is set to a positive value, then some debugging information will be printed
#' as the processing is done.
#'
#' @return A copy of `x` but with each of the objects within its
#' `data` slot having been passed through [oce::handleFlags,argo-method()].
#'
#' @examples
#' # Demonstrate applyQC to a built-in file
#' library(argoFloats)
#' f <- system.file("extdata", "SR2902204_131.nc", package="argoFloats")
#' raw <- readProfiles(f)
#' clean <- applyQC(raw)
#' oldpar <- par(no.readonly=TRUE)
#' par(mar=c(3.3, 3.3, 1, 1), mgp=c(2, 0.7, 0))
#' plot(raw, col="red", which="TS")
#' points(clean[[1]][["SA"]], clean[[1]][["CT"]], pch=20)
#' legend("topleft", pch=20, cex=1,
#'     col=c("black", "red"), legend=c("OK", "Flagged"), bg="white")
#' par(oldpar)
#'
#' @references
#' Carval, Thierry, Bob Keeley, Yasushi Takatsuki, Takashi Yoshida, Stephen Loch,
#' Claudia Schmid, and Roger Goldsmith. Argo User's Manual V3.3. Ifremer, 2019.
#' `doi:10.13155/29825`
#'
#' Kelley, D. E., Harbin, J., & Richards, C. (2021). argoFloats: An R package for analyzing
#' Argo data. Frontiers in Marine Science, (8), 636922. \doi{10.3389/fmars.2021.635922}
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
    if (is.null(flags))
        flags <- c(0, 3, 4, 6, 7, 9)
    ##message("next is flags:"); print(flags)
    if (is.null(actions))
        actions <- "NA"
    res@data$argos <- lapply(x@data$argos, oce::handleFlags, flags=flags, actions=actions, debug=debug)
    res@processingLog <- oce::processingLogAppend(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


#' Show Real-Time QC Test Results For an Argo Object
#'
#' `showQCTests` prints a summary of the quality-control (QC) tests
#' (if any) that were performed on an Argo profile in real-time (**Caution**: any tests completed and/or failed on delayed
#' mode data are not recorded).  It uses
#' [hexToBits()] to decode the hexadecimal  values that may
#' be stored in `historyQCTest`. From there it pairs the determined
#' test values with the appropriate actions, QC Tests performed or QC
#' Tests failed, found in `historyAction` within the `metadata` slot
#' of an individual Argo profile, as read directly with [oce::read.argo()]
#' or indirectly with [readProfiles()], the latter being illustrated in the
#' \dQuote{Examples} section below.  The \dQuote{Details}
#' section provides an explanation of how `showQCTests` works
#' at a low level, as an adjunct to the Argo documentation.
#' See section 3.3 of Kelley et al. (2021) for more on this function.
#'
#' The format used in the `historyQCTest` and `historyAction`
#' elements of the `metadata` slot of an [oce::argo-class] object
#' is mentioned in Sections 2.2.7, 2.3.7, 5.1, 5.3 and 5.4
#' of Carval et al. (2019), in which they are called
#' `HISTORY_QCTEST` and `HISTORY_ACTION`, respectively.
#' Both of these things are vectors of character values,
#' with the entries within `historyAction` providing names for
#' the entries within `historyQCTest`.
#'
#' In the context of `showQCTests`, the focus is on the element
#' of `historyAction` that equals `"QCP$"` (which maps to the element
#' of `historyQCTest` that specifies the QC tests that were
#' performed) and `"QCF$"` (which maps to the results of those
#' tests).  These mapped elements are character values providing
#' hexadecimal digits that are decoded with [hexToBits()], possibly after
#' lengthening the `historyQCTest` value matching`"QCF$"` by adding `"0"` digits
#' on the left to make the length be the same as that of the
#' `historyQCTest` value matching `"QCP$".
#'
#' The bits decoded from the relevant elements of `historyQCTest`
#' correspond to QC tests as indicated in the following table.
#' This is based on Table 11 of Carval et al. (2019),
#' after correcting the "Number" for test 18 from
#' 261144 to 262144, because the former is not an
#' integral power of 2, suggesting a typo
#' in the manual (since the whole point of the numerical scheme
#' is that the individual bits map to individual tests).
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
#' 18  |  262144 |  Frozen profile test
#' 19  |  524288 |  Deepest pressure test
#' 20  | 1048576 |  Questionable Argos position test
#' 21  | 2097152 |  Near-surface unpumped CTD salinity test
#' 22  | 4194304 |  Near-surface mixed air/water test
#' 23  | 8388608 |  Interim rtqc flag scheme for data deeper than 2000 dbar
#' 24  |16777216 | Interim rtqc flag scheme for data from experimental sensors
#' 25  |33554432 | MEDD test
#'
#' @param x an [oce::argo-class] object, as read directly with [oce::read.argo()]
#' or as extracted from the return value of a call to [readProfiles()], as
#' in the \dQuote{Examples}.
#'
#' @param style a character value governing the output printed by `showQCFlags`,
#' either `"brief"` (the default) for a single line stating all the tests by
#' numbers, followed by lines giving the number and description of all failed tests,
#' or `"full"` for a listing of each test that was performed, with an indication
#' of whether `x` passes or fails it.
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
#' Carval, Thierry, Bob Keeley, Yasushi Takatsuki, Takashi Yoshida, Stephen Loch,
#' Claudia Schmid, and Roger Goldsmith. Argo User's Manual V3.3. Ifremer, 2019.
#' `doi:10.13155/29825`
#'
#' Kelley, D. E., Harbin, J., & Richards, C. (2021). argoFloats: An R package for analyzing
#' Argo data. Frontiers in Marine Science, (8), 636922.
#' \doi{10.3389/fmars.2021.635922}
#'
#' @export
#' @author Jaimie Harbin and Dan Kelley
showQCTests <- function(x, style="brief")
{
    QCTests <- c("Platform Identification test"=1,
                 "Impossible Date test"=2,
                 "Impossible Location test"=3,
                 "Position on Land test"=4,
                 "Impossible Speed test"=5,
                 "Global Range test"=6,
                 "Regional Global Parameter test"=7,
                 "Pressure Increasing test"=8,
                 "Spike test"=9,
                 "Top and Bottom Spike test (obsolete)"=10,
                 "Gradient test"=11,
                 "Digit Rollover test"=12,
                 "Stuck Value test"=13,
                 "Density Inversion test"=14,
                 "Grey List test"=15,
                 "Gross Salinity or Temperature Sensor Drift test"=16,
                 "Visual QC test"=17,
                 "Frozen profile test"=18,
                 "Deepest pressure test"=19,
                 "Questionable Argos position test"=20,
                 "Near-surface unpumped CTD salinity test"=21,
                 "Near-surface mixed air/water test"=22,
                 "Interim rtqc flag scheme for data deeper than 2000 dbar"=23,
                 "Interim rtqc flag scheme for data from experimental sensors"=24,
                 "MEDD test"=25)
    if (!inherits(x, 'argo'))
        stop("can only display Quality Control tests for oce objects of 'argo' class")
    ## Only attempt a display if the object holds HISTORY_ACTION and HISTORY_TESTS
    ## Permit both SNAKE_CASE and camelCase names; oce switched to the latter 2020 Jun 24.
    mnames <- names(x@metadata)
    if ("HISTORY_ACTION" %in% mnames)
        action <- x[["HISTORY_ACTION"]]
    else if ("historyAction" %in% mnames)
        action <- x[["historyAction"]]
    else {
        cat("historyAction is not present in the metadata for this cycle, so no report can be given")
        return(invisible(NULL))
    }
    if ("HISTORY_QCTEST" %in% mnames) # to handle oce before version 1.3-0
        tests <- x[["HISTORY_QCTEST"]]
    else if ("historyQCTest" %in% mnames)
        tests <- x[["historyQCTest"]]
    else
        return(invisible(NULL))
    if (is.null(tests))
        return(invisible(NULL))
    ## Match strings within 'action' to find the tests that were performed
    nrows <- nrow(tests)
    indent <- ""
    for (irow in seq_len(nrows)) {
        if (nrows > 1) {
            cat("Profile", irow, "of", nrows, "profiles\n")
            indent <- "    "
        }
        perf <- tests[irow, which(action[irow,] == "QCP$")]
        ## Match strings within 'action' to find the tests that failed
        fail <- tests[irow, which(action[irow,] == "QCF$")]
        ## Add zeros on left of 'fail', if needed to match length of 'perf'
        failFull <- paste0(paste(rep("0",nchar(perf)-nchar(fail)),collapse=""), fail, sep="")
        perfIndices <- which(1==hexToBits(perf))
        failIndices <- -1 + which(1==hexToBits(failFull))
        if (style == "brief") {
            cat(indent, "Tests performed: ", paste(QCTests[perfIndices], collapse=" "), "\n", sep="")
            if (length(failIndices)) {
                for (i in failIndices)
                    cat(indent, sprintf("    Failed test %2d (%s)\n", QCTests[i], names(QCTests)[i]))
            } else {
                cat(indent, "    Passed all tests\n")
            }
        } else if (style == "full") {
            cat(indent, "Test |  Status  | Description\n")
            cat(indent, "-----|----------|--------------------------------------------------------------\n")
            for (i in QCTests) {
                failed <- i %in% failIndices
                skipped <- !(i %in% perfIndices)
                status <- if (failed) "*Failed*" else if (skipped) " Skipped" else "  Passed"
                cat(indent, sprintf("%4d | %7s | %s\n", i, status, names(QCTests)[i]))
            }
            cat(indent, "-----|----------|--------------------------------------------------------------\n")
        } else {
            stop("style must be either \"brief\" or \"full\", not \"", style, "\" as given")
        }
    }
    invisible(NULL)
}

