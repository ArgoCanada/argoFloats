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
#' @param showTime logical value indicating whether to preface message with
#' the present time. This can be useful for learning about which operations
#' are using the most time.
#' @param unindent integer specifying the degree of reverse indentation
#' to be done, as explained in the \dQuote{Details} sction.
#' @importFrom utils flush.console
#' @export
#' @author Dan Kelley
argoFloatsDebug <- function(debug=0, ..., style="plain", showTime=TRUE, unindent=0)
{
    debug <- if (debug > 2) 2 else max(0, floor(debug + 0.5))
    if (debug > 0) {
        n <- 3 - debug - unindent
        if (is.character(style) && style == "plain") {
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(...)
        } else if (is.character(style) && style == "bold") {
            cat("\033[1m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(...)
            cat("\033[0m")
        } else if (is.character(style) && style == "italic") {
            cat("\033[3m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(...)
            cat("\033[0m")
        } else if (is.character(style) && style == "red") {
            cat("\033[31m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(...)
            cat("\033[0m")
        } else if (is.character(style) && style == "green") {
            cat("\033[32m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(...)
            cat("\033[0m")
        } else if (is.character(style) && style == "blue") {
            cat("\033[34m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(...)
            cat("\033[0m")
        } else if (is.function(style)) {
            if (n > 0)
                cat(style(paste(rep("  ", n), collapse="")))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
            cat(style(...))
        } else { # fallback
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            if (showTime)
                cat(format(Sys.time(), "[%H:%M:%S] "))
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
#' @author Dan Kelley
downloadWithRetries <- function(url, destdir=".", destfile=NULL, mode="wb", quiet=FALSE,
                                force=FALSE, retries=3, debug=0)
{
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
    argoFloatsDebug(debug, "    mode='", mode, "'", ", quiet=", quiet, ", force=", force, ",\n",
                    style="bold", sep="", unindent=1)
    argoFloatsDebug(debug, "    retries=", retries, ") {\n",
                    style="bold", sep="", unindent=1)
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



#' Get Data for an Argo Float Profile
#'
#' @param url character value giving the URLs for an Argo float a profile. (In
#' a future version, `url` may be permitted to be a vector.)
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
#' @examples
#'\dontrun{
#' # These examples assume that the ~/data/argo directory exists and is readable.
#' library(argoFloats)
#' library(oce)
#' #
#' # Example 1: get float profile based on URL
#' url <- "ftp://ftp.ifremer.fr/ifremer/argo/dac/nmdis/2901633/profiles/R2901633_071.nc"
#' file <- getProfiles(url=url, destdir="~/data/argo")
#' argo <- read.argo(file)
#' plot(argo, which=c(1, 4, 6, 5))
#' #
#' # Example 2: get float profile nearest Sable Island
#' theIndex <- getIndex(destdir="~/data/argo")
#' load(theIndex) # defines 'index'
#' lon0 <- -59.9149
#' lat0 <- 43.9337
#' dist <- geodDist(index$data$longitude, index$data$latitude, lon0, lat0)
#' which0 <- which.min(dist)
#' url <- paste0(index$ftpRoot, "/", index$data$file[which0])
#' file0 <- getProfiles(url=url, destdir="~/data/argo")
#' argo0 <- read.oce(file0)
#' plot(argo0, which=c(1, 4, 6, 5))
#'}
#' @importFrom curl curl_download
#' @export
#' @author Dan Kelley
getProfiles <- function(url=NULL, destdir=".", destfile, mode="wb",
                        retries=3, force=FALSE, quiet=FALSE, debug=0)
{
    argoFloatsDebug(debug,  "getProfiles(url=\"", url, "\", destdir=\"", destdir, "\", destfile=\"",
                    if (missing(destfile)) "(missing)" else destfile, "\", ...) {", sep="", "\n", style="bold", unindent=1)
    ## If the ID starts with ftp://, thn we just download the file directly, ignoring server
    if (!grepl("^ftp://", url))
        stop("the url must start with \"ftp://\" -- contact authors if you need this limitation to be lifted")
    if (missing(destfile)) {
        destfile <- gsub(".*/(.*).nc", "\\1.nc", url)
        argoFloatsDebug(debug,  "inferred destfile=\"", destfile, "\" from url.\n", sep="")
    }
    downloadWithRetries(url=url, destdir=destdir, destfile=destfile, mode=mode, quiet=quiet,
                        force=force, retries=retries, debug=debug-1)
    argoFloatsDebug(debug,  "} # getProfiles()", sep="", "\n", style="bold", unindent=1)
    paste0(destdir, "/", destfile)
}


#' Get an Index of Available Argo Float Profiles
#'
#' This function gets an index of available Argo float profiles, either by
#' by downloading information from a data repository or by resusing an index
#' (stored as an `.rda` file) that was prepared by a recent call to the function.
#'
#' The first step is to construct a URL for downloading, based on the
#' `url` and `file` arguments. That URL will be a string ending in `.gz`,
#' and from this the name of a local file is constructed by changing the
#' suffix to `.rda` and prepending the file directory specified by
#' `destdir`.  If an `.rda` file of that name already exists, and is less
#' than `age` days old, then no downloading takes place. This caching
#' procedure is a way to save time, because the download can take from a
#' minute to an hour, depending on the bandwidth of the connection to the
#' server.
#'
#' The resultant `.rda` file, which is named in the return value of this
#' function, holds a list named `index` that holds following elements:
#' * `ftpRoot`, the FTP root stored in the header of the source `file`
#'    (see next paragraph).
#' * `server`, the argument provided here.
#' * `file`, the argument provided here.
#' * `header`, the preliminary lines in the source file that start
#'    with the `#` character.
#' * `data`, a data frame containing the items in the source file.
#'    This has columns named `file`, `date`, `longitude`, `latitude`,
#'    `ocean`, `profiler_type`, `institution`, and `date_update`.
#'
#' Note that `paste0(argoIndex$ftpRoot, argoIndex$data$file)` will
#' form a vector of names of files that can be downloaded with
#' [getProfiles()] and then analyzed and plotted with functions
#' provided by the \CRANpkg{oce} package.
#'
#' @template server
#' @param file character value indicating the file on the server, also
#' used as a pattern for the name of a constructed `.rda` file that
#' is placed in the `destdir` directory.
#' For the `ftp://usgodae.org/pub/outgoing/argo` server,
#' two of multiple choices for `file` are
#' `ar_index_global_prof.txt.gz`
#' (which is the default for this function) and
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
#' @return An object of class `argoFloats` with type=`"index"`.
#'
#' @examples
#'\dontrun{
#' library(argoFloats)
#' library(oce)
#' # Example: Temporal and spatial coverage of BIO-Argo floats.
#' par(mfrow=c(2, 1), mar=c(3, 3, 1, 1))
#' ai <- getIndex(file="argo_bio-profile_index.txt.gz", destdir="~/data/argo")
#' summary(ai)
#' hist(ai[["date"]], breaks="years", main="", xlab="Time", freq=TRUE)
#' data(coastlineWorld)
#' par(mar=rep(0.5, 4))
#' mapPlot(coastlineWorld, col="gray")
#' mapPoints(ai[["longitude"]], ai[["latitude"]], pch=".", col="blue")
#'}
#'
#' @author
#' Dan Kelley
#'
#' @family functions related to argo data
#'
#' @importFrom utils read.csv tail
#' @importFrom curl curl_download
#' @importFrom oce processingLogAppend
#' @export
#' @author Dan Kelley
getIndex <- function(server="ftp://usgodae.org/pub/outgoing/argo",
                     file="ar_index_global_prof.txt.gz",
                     destdir=".",
                     age=7,
                     quiet=FALSE, debug=0)
{
    if (!requireNamespace("curl", quietly=TRUE))
        stop('must install.packages("curl") to download Argo data')
    res <- new("argoFloats", type="index")
    ## Sample file
    ## ftp://ftp.ifremer.fr/ifremer/argo/dac/aoml/1900710/1900710_prof.nc
    ## ftp://usgodae.org/pub/outgoing/argo/dac/aoml/1900710/1900710_prof.nc
    argoFloatsDebug(debug,  "getIndex(server=\"", server, "\", file=\"", file, "\"", ", destdir=\"", destdir, "\") {", sep="", "\n", style="bold", showTime=FALSE, unindent=1)
    url <- paste(server, file, sep="/")
    destfile <- paste(destdir, file, sep="/")
    ## NOTE: we save an .rda file, not the .gz file, for speed of later operations
    destfileRda <- gsub(".gz$", ".rda", destfile)
    res@metadata$url <- url
    res@metadata$header <- NULL
    res@metadata$file <- destfileRda

    ## See if we have an .rda file that is sufficiently youthful.
    if (file.exists(destfileRda)) {
        destfileAge <- (as.integer(Sys.time()) - as.integer(file.info(destfileRda)$mtime)) / 86400 # in days
        if (destfileAge < age) {
            argoFloatsDebug(debug, "The local .rda file\n    ", destfileRda, "\nis not being updated from\n    ", url, "\nbecause it is only", round(destfileAge, 4), "days old.\n", showTime=FALSE)
            argoFloatsDebug(debug, "about to load '", destfileRda, "'.\n", sep="")
            load(destfileRda)
            argoFloatsDebug(debug, "finished loading '", destfileRda, "'.\n", sep="")
            res@metadata$server <- server
            res@metadata$file <- file
            res@metadata$destfileRda <- destfileRda
            res@metadata$server <- argoFloatsIndex$server
            res@metadata$ftpRoot <- argoFloatsIndex$ftpRoot
            res@metadata$header <- argoFloatsIndex$header
            res@data$index <- argoFloatsIndex$index
            argoFloatsDebug(debug, "} # getIndex()\n", style="bold", showTime=FALSE, unindent=1)
            return(res)
        }
    }
    ## We need to download data. We do that to a temporary file, because we will be saving
    ## an .rda file, not the data on the server.
    destfileTemp <- tempfile(pattern="argo", fileext=".gz")
    argoFloatsDebug(debug, "downloading temporary index file\n    ", destfileTemp, "\nfrom\n    ", url, "\n", sep="")
    curl::curl_download(url=url, destfile=destfileTemp, quiet=quiet, mode="wb")
    argoFloatsDebug(debug, "about to read header.\n", sep="")
    first <- readLines(destfileTemp, 100)
    hash <- which(grepl("^#", first))
    ftpRoot <- gsub("^[^:]*:[ ]*(.*)$", "\\1", first[which(grepl("^# FTP", first))[1]])
    header <- first[hash]
    lastHash <- tail(hash, 1)
    names <- strsplit(first[1 + lastHash], ",")[[1]]
    argoFloatsDebug(debug, "about to read the newly-downloaded index file.\n", sep="")
    index <- read.csv(destfileTemp, skip=2 + lastHash, col.names=names, stringsAsFactors=FALSE)
    argoFloatsDebug(debug, "setting out-of-range latitude and longitude to NA.\n", sep="")
    if ("latitude" %in% names(index))
        index$latitude[abs(index$latitude) > 90] <- NA
    if ("longitude" %in% names(index))
        index$longitude[abs(index$longitude) > 360] <- NA
    argoFloatsDebug(debug, "decoding dates.\n", sep="")
    index$date <- as.POSIXct(as.character(index$date), format="%Y%m%d%H%M%S", tz="UTC")
    index$date_update <- as.POSIXct(as.character(index$date_update), format="%Y%m%d%H%M%S",tz="UTC")
    argoFloatsDebug(debug,  "saving cache file '", destfileRda, "'.\n", sep="")
    argoFloatsIndex <- list(ftpRoot=ftpRoot, server=server, file=file, header=header, index=index)
    save(argoFloatsIndex, file=destfileRda)
    argoFloatsDebug(debug,  "removing temporary file '", destfileTemp, "'.\n", sep="")
    unlink(destfileTemp)
    res@metadata$server <- server
    res@metadata$file <- file
    res@metadata$destfileRda <- destfileRda
    res@metadata$server <- argoFloatsIndex$server
    res@metadata$ftpRoot <- argoFloatsIndex$ftpRoot
    res@metadata$header <- argoFloatsIndex$header
    res@data$index <- argoFloatsIndex$index
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("getIndex(server='", server, "', file='", file, "', age=", age, ")", sep=""))
    argoFloatsDebug(debug, "} # getIndex()\n", style="bold", unindent=1)
    res
}
