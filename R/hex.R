#' Convert a hex digit to 4 integers indicating the bits, in 'math' order.
#' 
#' This function converts hex digits to 4 integers indicating bits, in 'math' order. It's
#' intended to be used to look at the hex digits of `HISTORY_QCTEST` of an [`argoFloats-class`]
#' object that was created by [readProfiles()]. It then converts these hex digits to bits to
#' be used within the [showQCTests()] function.
#' 
#' @param x a character value corresponding to a hex digit (i.e. 0 through 9, or A through F)
#' @return an integer vector
#' @examples
#' \dontrun{
#' hexToNibble('2')
#' hexToNibble('e')
#' }
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
