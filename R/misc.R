## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Convert Hexadecimal Digit to Integer Vector
#'
#' `hexToBits` converts a string holding hexadecimal digits to a sequence of integers
#' 0 or 1, for the bits.  This is mainly for for use within [showQCTests()].
#'
#' @param hex a vector of character values corresponding to a sequence of one or more
#' hexadecimal digits (i.e. `"0"` through `"9"`,`"a"` through `"f"`, or `"A"` through `"F"`).
#'
#' @return An integer vector holding the bits as values 0 or 1.  The
#' inverse of 'mathematical' order is used, as is the case for the base
#' R function [rawToBits()]; see the \dQuote{Examples}.
#'
#' @examples
#' library(argoFloats)
#' hexToBits("3") # 1 1 0 0
#' hexToBits("4000") # 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
#'
#' @export
#'
#' @author Jaimie Harbin and Dan Kelley
hexToBits <- function(hex) {
    ## see https://github.com/ArgoCanada/argoFloats/issues/176
    res <- NULL
    for (h in strsplit(hex, "")[[1]]) {
        bits <- ifelse(01 == rawToBits(as.raw(paste0("0x0", h))), 1, 0)
        res <- c(res, tail(rev(bits), 4))
    }
    rev(res)
}
