#' Show which QC tests were performed and failed on an argos class
#' 
#' This function uses integer value from [hexToNibble()] internally to
#' convert hex digits of `HISTORY_QCTEST` of a single [`argoFloats-class`]
#' object that was created by [readProfiles()] to indicate which QC tests
#' were performed and/or failed.
#'
#' @param a a single float of [`argoFloats-class`]
#' @return The associated quality-control (QC) test numbers performed and failed.
#' @examples
#' \dontrun{
#' library(argoFloats)
#' data('index')
#' subset <- subset(index, 1)
#' profiles <- getProfiles(subset)
#' argos <- readProfiles(profiles)
#' argos1 <- argos[[1]]
#' showQCTests(argos[[1]])
#'   } 
showQCTests <- function(a)
{
    if (!inherits(a, 'argo'))
        stop("can only display Quality Control tests for oce objects of 'argo' class")
    ## Only attempt a display if the object holds HISTORY_ACTION and HISTORY_TESTS
    action <- a[['HISTORY_ACTION']]
    if (is.null(action))
        return(invisible())
    tests <- a[['HISTORY_QCTEST']]
    if (is.null(tests))
        return(invisible())
    ## Match strings within 'action' to find the tests that were performed
    perf <- tests[1, which(action == "QCP$")]
    ## Match strings within 'action' to find the tests that failed
    fail <- tests[1, which(action == "QCF$")]
    ## Break strings into characters
    perfChars <- strsplit(perf, "")[[1]]
    failChars <- strsplit(fail, "")[[1]]
    perfIndices <- which(1 == unlist(lapply(perfChars, function(hex) hexToNibble(hex))))
    failIndices <- which(1 == unlist(lapply(failChars, function(hex) hexToNibble(hex))))
    cat("Tests performed:", paste(perfIndices, collapse=" "), "\n")
    cat("Tests failed:   ", paste(failIndices, collapse=" "), "\n")
    invisible()
}
