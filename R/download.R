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
downloadWithRetries <- function(url, destdir, destfile, quiet=FALSE,
                                age=argoDefaultProfileAge(), retries=3, debug=0)
{
    retries <- max(1, as.integer(retries))
    if (length(destfile) != length(url))
        stop("length(url)=", length(destfile), " must equal length(destfile)=", length(destfile))
    
    if (length(url) == 0)
        return(character(0))
    
    destination <- paste0(destdir, "/", destfile)
    
    destinationInfo <- file.info(destination)
    destinationAge <- (as.integer(Sys.time()) - as.integer(destinationInfo$mtime)) / 86400 # in days
    skipDownload <- file.exists(destination) & (destinationAge < age)
    
    urlDownload <- url[!skipDownload]
    destinationDownload <- destination[!skipDownload]
    successDownload <- rep(FALSE, length(urlDownload))
    
    if (length(urlDownload) == 0)
        return(destination)
    
    useProgressBar <- !quiet && interactive()
    if (useProgressBar)
        pb <- txtProgressBar(0, length(urlDownload), 0, style = 3)
    
    for (i in seq_along(urlDownload)) {
        success <- FALSE
        for (trial in seq_len(1 + retries)) {
            if (!quiet) message(sprintf("Downloading '%s'", urlDownload[i]))
            t <- try(curl::curl_download(url=urlDownload[i], destfile=destinationDownload[i]), silent=TRUE)
            if (inherits(t, "try-error") && any(grepl("application callback", t))) {
                stop(t)
            } else if (!inherits(t, "try-error")) {
                successDownload[i] <- TRUE
                break
            }
        }
        if (!successDownload[i]) {
            if (!quiet)
                message("failed download '", urlDownload[i], "'\n  after ", retries, " attempts.\n  Try running getIndex(age=0) to refresh the index, in case a file name changed.")
        }
        if (useProgressBar)
            setTxtProgressBar(pb, i)
    }
    
    if (useProgressBar)
        close(pb)
    
    # collect failed downloads set destination to NA where this occurred
    failedDownloads <- destinationDownload[!successDownload]
    destination[destination %in% failedDownloads] <- NA_character_
    
    # return vector of filenames downloaded
    destination
}
