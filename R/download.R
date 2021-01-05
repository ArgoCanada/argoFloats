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
#' @importFrom utils unzip
#'
#' @export
#'
#' @author Dan Kelley
downloadWithRetries <- function(url, destdir=argoDefaultDestdir(), destfile=NULL, quiet=FALSE,
                                age=argoDefaultProfileAge(), retries=3, debug=0)
{
    if (!requireNamespace("curl", quietly=TRUE))
        stop("must install.packages(\"curl\") for downloadWithRetries() to work")
    if (missing(url))
        stop("must specify url")
    if (length(destdir) > 1)
        stop("destdir must be of length 1")
    retries <- max(1, as.integer(retries))
    argoFloatsDebug(debug, "downloadWithRetries(\n",
                    style="bold", sep="", unindent=1)
    argoFloatsDebug(debug, "    url='", paste(url, collapse="', '"), "',\n",
                    style="bold", sep="", unindent=1)
    argoFloatsDebug(debug, "    destdir='", destdir, "',\n",
                    style="bold", sep="", unindent=1)
    argoFloatsDebug(debug, "    destfile='", paste(destfile, collapse="', '"), "',\n",
                    style="bold", sep="", unindent=1)
    argoFloatsDebug(debug, "    quiet=", quiet, ", retries=", retries, ") {\n",
                    style="bold", sep="", unindent=1)
    n <- length(url)
    if (length(destfile) != n)
        stop("length(url)=", n, " must equal length(destfile)=", length(destfile))
    for (i in 1:n) {
        destination <- paste0(destdir, "/", destfile[i])
        if (file.exists(destination)) {
            destinationAge <- (as.integer(Sys.time()) - as.integer(file.info(destination)$mtime)) / 86400 # in days
            argoFloatsDebug(debug, "destinationAge=", destinationAge, "\n", sep="")
        }
        if (file.exists(destination) && destinationAge < age) {
            argoFloatsDebug(debug, "Skipping \"", destination, "\" because it already exists and its age, ", destinationAge, "d, is under specified age=", age, "d\n", sep="")
        } else {
            success <- FALSE
            for (trial in seq_len(1 + retries)) {
                if (!quiet) message(sprintf("Downloading '%s'", url))
                t <- try(curl::curl_download(url=url, destfile=destination), silent=TRUE)
                if (inherits(t, "try-error") && any(grepl("application callback", t))) {
                    stop(t)
                } else if (inherits(t, "try-error")) {
                    argoFloatsDebug(debug, "failed download from \"", url, "\" ", if (trial < (1+retries)) "(will try again)\n" else "(final attempt)\n", sep="")
                } else {
                    argoFloatsDebug(debug, "successful download from \"", url, "\"\n", sep="")
                    success <- TRUE
                    break
                }
            }
            if (!success) {
                if (!quiet)
                    message("failed download '", url, "'\n  after ", retries, " attempts.\n  Try running getIndex(age=0) to refresh the index, in case a file name changed.")
                return(NA)
            }
            if (1 == length(grep(".zip$", destfile[i]))) {
                destinationClean <- gsub(".zip$", "", destination[i])
                unzip(destination[i], exdir=destinationClean)
                destination[i] <- destinationClean
                argoFloatsDebug(debug, "  Downloaded and unzipped into '", destination[i], "'\n", sep="")
            } else {
                argoFloatsDebug(debug, "  Downloaded file stored as '", destination[i], "'\n", sep="")
            }
        }
    }
    argoFloatsDebug(debug, "} # downloadWithRetries()\n", style="bold", unindent=1)
    destination
}

