#' dc: A Package to download and cache data files from the web
#' @docType package
#' @name argoFloats
NULL

#' Possibly print debugging information.
#'
#' This function is intended mainly for use within the package, but users may
#' also call it directly in their own code.  Within the package, the value
#' of `debug` is generally reduced by 1 on each nested function call, leading
#' to indented messages. Most functions start and end with a call to
#' [argoFloatsDebug()] that has `style="bold"` and `unindent=1`.
#'
#' @param debug an integer specifying the level of debugging. Values greater
#' than zero indicate that some printing should be done. Many functions
#' @param ... content to be printed, analogous to the the ... argument
#' list of [cat()].
#' @param style character value indicating special formatting, with `"plain"`
#' for normal text, `"bold"` for bold-faced text, `"italic"` for italicized
#' text, `"red"` for red text, `"green"` for green text, or `"blue"` for blue
#' text. These codes may not be combined.
#' @param unindent integer specifying the degree of reverse indentation
#' to be done, as explained in the \dQuote{Details} sction.
#' @importFrom utils flush.console
#' @export
argoFloatsDebug <- function(debug=0, ..., style="plain", unindent=0)
{
    debug <- if (debug > 4) 4 else max(0, floor(debug + 0.5))
    if (debug > 0) {
        n <- 5 - debug - unindent
        if (is.character(style) && style == "plain") {
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
        } else if (is.character(style) && style == "bold") {
            cat("\033[1m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
            cat("\033[0m")
        } else if (is.character(style) && style == "italic") {
            cat("\033[3m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
            cat("\033[0m")
        } else if (is.character(style) && style == "red") {
            cat("\033[31m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
            cat("\033[0m")
        } else if (is.character(style) && style == "green") {
            cat("\033[32m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
            cat("\033[0m")
        } else if (is.character(style) && style == "blue") {
            cat("\033[34m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
            cat("\033[0m")
        } else if (is.function(style)) {
            if (n > 0)
                cat(style(paste(rep("  ", n), collapse="")))
            cat(style(...))
        } else { # fallback
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
        }
        flush.console()
    }
    invisible()

}


#' Download and Cache a Dataset
#'
#' General function for downloading and caching a dataset.
#'
#' @template url
#' @template destdir
#' @template destfile
#' @template mode
#' @template quiet
#' @template force
#' @template retries
#' @template debug
#'
#' @return String indicating the full pathname to the downloaded file.
#' @importFrom utils unzip
#' @importFrom curl curl_download
#' @export
downloadWithRetries <- function(url, destdir=".", destfile=NULL, mode="wb", quiet=FALSE,
                                force=FALSE, retries=3, debug=0)
{
    if (missing(url))
        stop("must specify url")
    if (length(destdir) > 1)
        stop("destdir must be of length 1")
    retries <- max(1, as.integer(retries))
    argoFloatsDebug(debug, "downloadWithRetries(url='", paste(url, collapse="', '"), "', ",
                    "                    destdir='", destdir, "',\n",
                    "                    destfile='", paste(destfile, collapse="', '"), "',\n",
                    "                    mode='", mode, "',\n",
                    "                    quiet=", quiet, ",\n",
                    "                    force=", force, ",\n",
                    "                    retries=", retries, ") {\n", style="bold", sep="", unindent=1)
    n <- length(url)
    if (length(destfile) != n)
        stop("length(url)=", n, " must equal length(destfile)=", length(destfile))
    for (i in 1:n) {
        destination <- paste0(destdir, "/", destfile[i])
        if (!force && file.exists(destination)) {
            argoFloatsDebug(debug, "Skipping \"", destination, "\" because it already exists\n", sep="")
        } else {
            success <- FALSE
            for (trial in seq_len(1 + retries)) {
                t <- try(curl::curl_download(url=url, destfile=destination, quiet=quiet, mode=mode))
                if (!inherits(t, "try-error")) {
                    success <- TRUE
                    break
                }
            }
            if (!success)
                stop("failed to download, after ", retries, " retries")
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



#' Download and cache an argo float file specified by URL
#'
#' @param url character value giving the Argo float URL.
#' @template destdir
#' @param destfile optional character value that specifies the name to be used
#' for the downloaded file. If this is not specified, then a name is determined
#' from the value of `url`.
#' @template mode
#' @template retries
#' @template force
#' @template quiet
#' @template debug
#'
#' @export
#' @examples
#'\dontrun{
#' # These examples assume that the ~/data/argo directory exists and is readable.
#' library(argoFloats)
#' url <- "ftp://ftp.ifremer.fr/ifremer/argo/dac/nmdis/2901633/profiles/R2901633_071.nc"
#' file <- downloadByUrl(url, destdir="~/data/argo")
#' argo <- read.oce(file)
#' summary(argo) # an oce object of class 'argo'
#' par(mfrow=c(2, 2))
#' library(oce)
#' plot(argo, which="map")
#' mtext(argo[["time"]], cex=par("cex"))
#' mtext(gsub(".*/", "", file), cex=par("cex"), line=-1)
#' plot(argo, which="TS")
#' plot(argo, which="temperature profile")
#' plot(argo, which="salinity profile")
#'}
#' @importFrom curl curl_download
#' @export
downloadByUrl <- function(url=NULL, destdir=".", destfile, mode="wb",
                          retries=3, force=FALSE, quiet=FALSE, debug=0)
{
       argoFloatsDebug(debug,  "downloadByUrl(url=\"", url, "\", destdir=\"", destdir, "\", destfile=\"",
           if (missing(destfile)) "(missing)" else destfile, "\", ...) {", sep="", "\n", style="bold", unindent=1)
    ## If the ID starts with ftp://, thn we just download the file directly, ignoring server
    if (!grepl("^ftp://", url))
        stop("the url must start with \"ftp://\" -- contact authors if you need this limitation to be lifted")
    if (missing(destfile)) {
        destfile <- gsub(".*/(.*).nc", "\\1.nc", url)
        argoFloatsDebug(debug,  "inferred destfile=\"", destfile, "\" from url.\n", sep="")
    }
    downloadWithRetries(url=url, destdir=destdir, destfile=destfile, mode=mode, quiet=quiet,
                        force=force, retries=retries)
    argoFloatsDebug(debug,  "} # downloadByUrl()", sep="", "\n", style="bold", unindent=1)
    destfile
}


#' Get an index of available floats
#'
#' Downloads a file (or uses a cached file) and then reads it to create a
#' local `.rda` file that stores information about available float files,
#' as explained in \dQuote{Details}.
#' The download takes several of order 1 to 60 minutes, so this function
#' has an `age` argument that lets the user avoid new downloads of
#' data that were downloaded recently.
#'
#' The first step is to construct a URL for downloading, based on the
#' `url` and `file` arguments. That URL will be a string ending in `.gz`,
#' and from this the name of a local file is constructed by changing the
#' suffix to `.rda`. If that rda file is less than the age (in days)
#' specified by the `age` argument, then no downloading takes place,
#' and [downloadIndex()] returns the name of that rda file.
#'
#' However, if the local rda file is older than `age` days, a download
#' is started, using [curl::curl_download()] from the \CRANpkg{curl}
#' package.  The data file is downloaded to a local temporary file,
#' and then the contents of that file are analysed, with the results
#' of the analysis being stored in the local rda file.
#'
#' The resultant `.rda` file holds a list named `argoIndex`
#' that holds following elements:
#' * `ftpRoot`, the FTP root  stored in the header of the source `file`.
#' * `server`, the argument  provided here.
#' * `file`, the argument provided here.
#' * `header`, the preliminary lines in the source file that start with the `#` character.
#' * `data`, a data frame containing the items in the source file. As of
#'    files downloaded in February 2020, this has columns named
#'    `file`, `date`, `longitude`, `latitude`, `ocean`, `profiler_type`,
#'    `institution`, and `date_update`.
#'
#' Note that `paste0(argoIndex$ftpRoot, argoIndex$data$file)` will
#' form a vector of names of files that can be downloaded as local
#' Netcdf files (ending in suffix `.nc`) that can be read with
#' [oce::read.argo()] in the \CRANpkg{oce} packag, creating an
#' `argo` object.
#'
#' @template server
#' @param file character value indicating the file on the server, also
#' used as a pattern for the name of a constructed `.rda` file that
#' is placed in the `destdir` directory.
#' For the `ftp://usgodae.org/pub/outgoing/argo` server,
#' two of multiple choices for `file` are
#' `ar_index_global_prof.txt.gz`
#' and
#' `argo_bio-profile_index.txt.gz`
#' but examination of the server will reveal other possibilities
#' that might be worth exploring.
#' @template destdir
#' @param age numeric value indicating how old a downloaded file
#' must be (in days), for it to be considered out-of-date.  The
#' default, `age=7`, limits downloads to once per week, as a way
#' to avoid slowing down a workflow with a download that might take
#' a sizeable fraction of an hour. Set `age=0` to force a download,
#' regardless of the file age.
#' @param quiet silence some progress indicators.  The default
#' is to show such indicators.
#' @template debug
#'
#' @return A character value holding the name of the `.rda` file,
#' which is typically loaded with [load()]; see \dQuote{Examples}.
#'
#' @examples
#'\dontrun{
#' # These examples assume that the ~/data/argo directory exists and is readable.
#' # Download whole index
#' ai <- downloadIndex(destdir="~/data/argo")
#' load(ai) # places argoIndex, a list, within the current workspace
#' # Plot histograms of 'date' and 'date_update'
#' par(mfrow=c(2, 1), mar=c(3, 3, 1, 1))
#' hist(argoIndex$data$date, breaks="years",
#'      main="", xlab="Time", freq=TRUE)
#' hist(argoIndex$data$date_update, breaks="years",
#'      main="", xlab="Last Update Time", freq=TRUE)
#' # Download and plot the last file in the index
#' url <- paste0(argoIndex$ftpRoot, "/", tail(argoIndex$data$file, 1))
#' file <- downloadByUrl(url, destdir="~/data/argo")
#' library(oce)
#' argo <- read.oce(file)
#' summary(argo)
#' par(mfrow=c(2, 2))
#' plot(argo, which="map")
#' mtext(argo[["time"]], cex=par("cex"))
#' plot(argo, which="TS")
#' plot(argo, which="temperature profile")
#' plot(argo, which="salinity profile")
#'}
#'
#' @author
#' Dan Kelley
#'
#' @family functions related to argo data
#'
#' @importFrom utils read.csv tail
#' @importFrom curl curl_download
#' @export
downloadIndex <- function(server="ftp://usgodae.org/pub/outgoing/argo",
                          file="ar_index_global_prof.txt.gz",
                          destdir=".",
                          age=7,
                          quiet=FALSE, debug=0)
{
    if (!requireNamespace("curl", quietly=TRUE))
        stop('must install.packages("curl") to download Argo data')
    ## Sample file
    ## ftp://ftp.ifremer.fr/ifremer/argo/dac/aoml/1900710/1900710_prof.nc
    ## ftp://usgodae.org/pub/outgoing/argo/dac/aoml/1900710/1900710_prof.nc
    argoFloatsDebug(debug,  "downloadIndex(server=\"", server, "\", file=\"", file, "\"", ", destdir=\"", destdir, "\") {", sep="", "\n", style="bold", unindent=1)
    url <- paste(server, file, sep="/")
    destfile <- paste(destdir, file, sep="/")
    ## NOTE: we save an .rda file, not the .gz file, for speed of later operations
    destfileRda <- gsub(".gz$", ".rda", destfile)
    ## See if we have an .rda file that is sufficiently youthful.
    if (file.exists(destfileRda)) {
        destfileAge <- (as.integer(Sys.time()) - as.integer(file.info(destfileRda)$mtime)) / 86400 # in days
        if (destfileAge < age) {
            argoFloatsDebug(debug, "The local .rda file\n    ", destfileRda, "\nis not being updated from\n    ", url, "\nbecause it is only", round(destfileAge, 4), "days old.\n")
            return(destfileRda)
        }
    }
    ## We need to download data. We do that to a temporary file, because we will be saving
    ## an .rda file, not the data on the server.
    destfileTemp <- tempfile(pattern="argo", fileext=".gz")
    argoFloatsDebug(debug,  format(Sys.time(), "[%H:%M:%S]"), " downloading temporary index file\n    ", destfileTemp, "\nfrom\n    ", url, "\n", sep="")
    curl::curl_download(url=url, destfile=destfileTemp, quiet=quiet, mode="wb")
    argoFloatsDebug(debug,  format(Sys.time(), "[%H:%M:%S]"), " about to read header.\n", sep="")
    first <- readLines(destfileTemp, 100)
    hash <- which(grepl("^#", first))
    ftpRoot <- gsub("^[^:]*:[ ]*(.*)$", "\\1", first[which(grepl("^# FTP", first))[1]])
    header <- first[hash]
    lastHash <- tail(hash, 1)
    names <- strsplit(first[1 + lastHash], ",")[[1]]
    argoFloatsDebug(debug,  format(Sys.time(), "[%H:%M:%S]"), " about to read the newly-downloaded index file.\n", sep="")
    data <- read.csv(destfileTemp, skip=2 + lastHash, col.names=names, stringsAsFactors=FALSE)
    argoFloatsDebug(debug,  format(Sys.time(), "[%H:%M:%S]"), " removing temporary file '", destfileTemp, "'.\n", sep="")
    unlink(destfileTemp)
    argoFloatsDebug(debug,  format(Sys.time(), "[%H:%M:%S]"), " about to decode dates.\n", sep="")
    data$date <- as.POSIXct(as.character(data$date), format="%Y%m%d%H%M%S", tz="UTC")
    data$date_update <- as.POSIXct(as.character(data$date_update), format="%Y%m%d%H%M%S",tz="UTC")
    argoFloatsDebug(debug,  format(Sys.time(), "[%H:%M:%S]"), " about to save to rda file '", destfileRda, "'.\n", sep="")
    argoIndex <- list(ftpRoot=ftpRoot, server=server, file=file, header=header, data=data)
    save(argoIndex, file=destfileRda)
    argoFloatsDebug(debug, format(Sys.time(), "[%H:%M:%S]"), " finished saving results to '", destfileRda, "'.\n", sep="")
    argoFloatsDebug(debug, "} # downloadIndex()\n", style="bold", unindent=1)
    destfileRda
}
