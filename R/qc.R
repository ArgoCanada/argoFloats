## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Use quality-control flags to alter argoFloats data
#'
#' This function examines the quality-control (QC) flags within an [argoFloats-class]
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
#' @param x An [argoFloats-class] object of type `"argos"`, as created by
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
#' library(argoFloats)
#' # Contrast TS diagrams for raw and flag-handled data
#' data(index)
#' i <- subset(index, 1:5) # first 5 profiles
#' raw <- readProfiles(getProfiles(i))
#' clean <- applyQC(raw)
#' par(mfrow=c(1, 2))
#' plot(raw, which="TS")
#' plot(clean, which="TS")
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
