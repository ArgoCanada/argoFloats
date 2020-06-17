library(argoFloats)
options(warn=1) # for debugging

data('index')
s <- subset(index,100)
gp <- getProfiles(s)
rp <- readProfiles(gp)
rp1 <- rp[[1]]


#' Convert a single hex digit to 4 integers indicating the bits, in 'math' order
#"
#' @param x a character value corresponding to a hex digit (i.e. 0 through 9, or A through F)
#' @return an integer vector
#' hexToNibble('2') # 0 0 1 0
#' hexToNibble('e') # 1 1 1 0
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
showQCTests(rp1)
