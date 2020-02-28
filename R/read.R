## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Read argo profiles, creating a list of argo objects
#'
#' @param profiles either a character vector holding the names
#' of local files to read, or an [argoFloat-class] object created
#' by either [getProfiles()] or [getIndex()]. In the last case,
#' the files must first have been downloaded either manually
#' or (preferably) with [getProfiles()].
#' @param debug an integer specifying the level of debugging. If
#' this is zero, the work proceeds silently. If it is 1,
#' a small amount of debugging information is printed.  Note that
#' `debug-1` is passed to [oce::read.argo()], which actually reads
#' the file, and so it will print messages if `debug` exceeds 1.
#' @export
#' @author Dan Kelley
readProfiles <- function(profiles, debug=0)
{
    debug <- floor(0.5 + debug)
    debug <- max(0, debug)
    res <- NULL
    message("FIXME(dek): show info on profiles here")
    if (is.character(profiles)) {
        res <- lapply(profiles, read.argo, debug=debug-1)
    } else {
        message("FIXME(dek): code me")
    }
    res
}

