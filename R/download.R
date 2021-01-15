## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Download and Cache a Dataset
#'
#' General function for downloading and caching a dataset.
#'
#' @template url
#'
#' @template destdir
#'
#' @template destfile
#'
#' @template quiet
#'
#' @template age
#'
#' @template retries
#'
#' @template debug
#'
#' @return A character value indicating the full pathname to the downloaded file,
#' or `NA`, if there was a problem with the download.
#'
## @importFrom curl curl_download
#'
#' @export
#'
#' @author Dan Kelley
downloadWithRetries <- function(url, destdir=argoDefaultDestdir(), destfile=NULL, quiet=FALSE,
                                age=argoDefaultProfileAge(), retries=3, debug=0)
{
    retries <- max(1, as.integer(retries))
    if (length(destfile) != length(url))
        stop("length(url)=", length(destfile), " must equal length(destfile)=", length(destfile))
    
    if (length(url) == 0)
        return(character(0))
    
    destination <- character(length(destfile))
    for (i in seq_along(url)) {
        destination[i] <- paste0(destdir, "/", destfile[i])
        if (file.exists(destination)) {
            destinationAge <- (as.integer(Sys.time()) - as.integer(file.info(destination)$mtime)) / 86400 # in days
        }
        if (!file.exists(destination) || (destinationAge < age)) {
            success <- FALSE
            for (trial in seq_len(1 + retries)) {
                if (!quiet) message(sprintf("Downloading '%s'", url))
                t <- try(curl::curl_download(url=url, destfile=destination), silent=TRUE)
                if (inherits(t, "try-error") && any(grepl("application callback", t))) {
                    stop(t)
                } else {
                    success <- TRUE
                    break
                }
            }
            if (!success) {
                if (!quiet)
                    message("failed download '", url, "'\n  after ", retries, " attempts.\n  Try running getIndex(age=0) to refresh the index, in case a file name changed.")
                return(NA)
            }
        }
    }
    
    destination
}
