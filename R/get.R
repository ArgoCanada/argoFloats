## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

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
                stop("failed to download from '", url, "', after ", retries, " retries")
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
#' @param url character value giving the URL for an Argo float a profile.
#' @template destdir
#' @param destfile optional character value that specifies the name to be used
#' for the downloaded file. If this is not specified, then a name is determined
#' from the value of `url`.
#' @template force
#' @template retries
#' @template quiet
#' @template debug
#'
#' @examples
#'\dontrun{
#' # These examples assume that the ~/data/argo directory exists.
#' library(argoFloats)
#' library(oce)
#'
#' # Example 1: a particular file
#' url <- "ftp://ftp.ifremer.fr/ifremer/argo/dac/nmdis/2901633/profiles/R2901633_071.nc"
#' file <- getProfileFromUrl(url=url, destdir="~/data/argo")
#' argo <- read.argo(file)
#' plot(argo, which=c(1, 4, 6, 5))
#'
#' # Example 2: argo profile nearest Sable Island
#' index <- getIndex(destdir="~/data/argo")
#' lon0 <- -59.9149
#' lat0 <- 43.9337
#' dist <- geodDist(index[["longitude"]], index[["latitude"]], lon0, lat0)
#' w <- which.min(dist)
#' url <- paste0(index[["metadata"]]["ftpRoot"], "/", index[["file"]][w])
#' fileSable <- getProfileFromUrl(url=url, destdir="~/data/argo")
#' argoSable <- read.oce(fileSable)
#' plot(argoSable, which=c(1, 4, 6, 5))
#'}
#'
#' @author Dan Kelley
#'
#' @importFrom curl curl_download
#' @export
getProfileFromUrl <- function(url=NULL, destdir=".", destfile=NULL,
                              force=FALSE, retries=3, quiet=FALSE, debug=0)
{
    argoFloatsDebug(debug,  "getProfileFromUrl(url=\"", url, "\", destdir=\"", destdir, "\", destfile=\"",
                    if (missing(destfile)) "(missing)" else destfile, "\", ...) {", sep="", "\n", style="bold", unindent=1)
    ## If the ID starts with ftp://, thn we just download the file directly, ignoring server
    if (!grepl("^ftp://", url))
        stop("the url must start with \"ftp://\" -- contact authors if you need this limitation to be lifted")
    if (is.null(destfile)) {
        destfile <- gsub(".*/(.*).nc", "\\1.nc", url)
        argoFloatsDebug(debug,  "inferred destfile=\"", destfile, "\" from url.\n", sep="")
    }
    downloadWithRetries(url=url, destdir=destdir, destfile=destfile, mode="wb", quiet=quiet,
                        force=force, retries=retries, debug=debug-1)
    argoFloatsDebug(debug,  "} # getProfileFromUrl()", sep="", "\n", style="bold", unindent=1)
    paste0(destdir, "/", destfile)
}


#' Get an Index of Available Argo Float Profiles
#'
#' This function gets an index of available Argo float profiles, either by
#' by downloading information from a data repository or by reusing an index
#' (stored as an `.rda` file) that was prepared by a recent call to the function.
#'
#' The first step is to construct a URL for downloading, based on the
#' `url` and `file` arguments. That URL will be a string ending in `.gz`,
#' or `.txt` and from this the name of a local file is constructed by changing the
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
#'    The names of these items are determined automatically from
#'    `"argo"` and `"bgcargo"` files, but for `"merged"` files,
#'    the header is malformed (as of February 2020) and so the names
#'    are set based on the authors' inspection of a downloaded file.
#'
#' Note that `paste0(argoIndex$ftpRoot, argoIndex$data$file)` will
#' form a vector of URLs pointing to argo netcdf files that can be
#' downloaded with any system tool for downloading files, but it
#' is more practical to use [getProfileFromUrl()], which also checks
#' to see if the data has already been constructed for a given URL.
#' Even better, one should use [getProfiles()], which computes
#' a set of relevant URLs based on the contents of the index, and
#' places the results in the `destfile` directory that was supplied
#' to [getIndex()], and stored in the index.
#'
#' Some expertise is required in deciding on the value for the
#' `file` argument to [getIndex()].  As of February 2020, the
#' FTP site `ftp://usgodae.org/pub/outgoing/argo` contains multiple
#' files that appear to be indices.  These are listed as the left-hand
#' column in the following table. The middle column is a "nickname"
#' for the file (and can be provided as the `file` argument to the
#' [getIndex()] function).  The right-hand column is a brief
#' description of the file contents, inferred by examination
#' of the file.  (Note that there some files on the server
#' have names similar to those given below, but ending in `.txt` instead
#' of `.txt.gz`, but these files take longer to download and
#' seem to be equivalent to the `.gz` versions, so [getIndex()] is
#' designed to work with them.)
#' \tabular{lll}{
#' *File Name*                       \tab *Nickname*              \tab *Contents*\cr
#' `ar_greylist.txt`                 \tab -                       \tab Suspious or malfunctioning float sensors.\cr
#' `ar_index_global_meta.txt.gz`     \tab -                       \tab Metadata files of the argo GDAC ftp site.\cr
#' `ar_index_global_prof.txt.gz`     \tab `"argo"`                \tab Argo data.\cr
#' `ar_index_global_tech.txt.gz`     \tab -                       \tab All technical files of the argo GDAC ftp site.\cr
#' `ar_index_global_traj.txt.gz`     \tab -                       \tab All trajectory files of the argo GDAC ftp site.\cr
#' `argo_bio-profile_index.txt.gz`   \tab `"bgc"` or `"bgcargo"`  \tab Biogeochemical Argo data (without S or T).\cr
#' `argo_bio-traj_index.txt.gz`      \tab -                       \tab Bio-trajectory files of the argo GDAC ftp site.\cr
#' `argo_merge-profile_index.txt.gz` \tab `"merge"` or `"merged"` \tab Merged `"argo"` and `"bgc"` data.\cr
#' }
#'
#' @template server
#' @param file character value that indicates the file name on the server, as in
#' the first column of the table given in \dQuote{Details}, or (for some file types)
#' as in the nickname given in the middle column. Note that the downloaded
#' file name will be based on the full file name given as this argument, and
#' that nicknames are expanded to the full filenames before saving.
#' @template destdir
#' @param age numeric value indicating how old a downloaded file
#' must be (in days), for it to be considered out-of-date.  The
#' default, `age=7`, limits downloads to once per week, as a way
#' to avoid slowing down a workflow with a download that might take
#' a sizable fraction of an hour. Set `age=0` to force a download,
#' regardless of the file age.
#' @param quiet silence some progress indicators.  The default
#' is to show such indicators.
#' @template debug
#'
#' @return An object of class [argoFloats-class] with type=`"index"`, which
#' is suitable as the first argument of [getProfiles()].
#'
#' @examples
#'\dontrun{
#' library(argoFloats)
#' library(oce)
#' # Example: Temporal and spatial coverage of merged argo/bgcargo measurements.
#' par(mfrow=c(2, 1), mar=c(3, 3, 1, 1))
#' ai <- getIndex(file="merged", destdir="~/data/argo")
#' summary(ai)
#' hist(ai[["date"]], breaks="years", main="", xlab="Time", freq=TRUE)
#' data(coastlineWorld)
#' par(mar=rep(0.5, 4))
#' mapPlot(coastlineWorld, col="gray")
#' mapPoints(ai[["longitude"]], ai[["latitude"]], pch=".", col="blue")
#'}
#'
#' @author Dan Kelley
#'
#' @importFrom utils read.csv tail
#' @importFrom curl curl_download
#' @importFrom oce processingLogAppend
#' @export
getIndex <- function(server="auto",
                     file="argo",
                     destdir=".",
                     age=7,
                     quiet=FALSE,
                     debug=0)
{
    ## Sample file
    ## ftp://ftp.ifremer.fr/ifremer/argo/dac/aoml/1900710/1900710_prof.nc
    ## ftp://usgodae.org/pub/outgoing/argo/dac/aoml/1900710/1900710_prof.nc
    if (!requireNamespace("curl", quietly=TRUE))
        stop('must install.packages("curl") to download Argo data')
    res <- new("argoFloats", type="index")
    res@metadata$destdir <- destdir
    argoFloatsDebug(debug,  "getIndex(server=\"", server, "\", file=\"", file, "\"", ", destdir=\"", destdir, "\") {", sep="", "\n", style="bold", showTime=FALSE, unindent=1)
    if (server == "auto") {
        server <- c("ftp://usgodae.org/pub/outgoing/argo",
                    "ftp://ftp.ifremer.fr/ifremer/argo")
        argoFloatsDebug(debug, 'server "auto" expanded to c("',
                        paste(server, collapse='", "'), '")\n', sep="")
    }
    ## Ensure that we can save the file
    if (!file.exists(destdir))
        stop("First, create a directory named '", destdir, "'")
    if (!file.info(destdir)$isdir)
        stop("'", destdir, "' is not a directory")
    ## Handle nicknames
    if (file == "argo") {
        file <- "ar_index_global_prof.txt.gz"
        argoFloatsDebug(debug, "Converted file=\"argo\" to file=\"", file, "\".\n", sep="")
    } else if (file == "bgcargo" || file == "bgc") {
        file <- "argo_bio-profile_index.txt.gz"
        argoFloatsDebug(debug, "Converted file=\"bgcargo\" to file=\"", file, "\".\n", sep="")
    } else if (file == "merge" || file == "merged") {
        file <- "argo_merge-profile_index.txt.gz"
        argoFloatsDebug(debug, "Converted file=\"argo_merge\" to file=\"", file, "\".\n", sep="")
    }
    url <- paste(server, file, sep="/")
    destfile <- paste(destdir, file, sep="/")
    ## NOTE: we save an .rda file, not the .gz file, for speed of later operations
    destfileRda <- gsub(".gz$", ".rda", destfile)
    destfileRda <- gsub(".txt$", ".rda", destfile)
    res@metadata$url <- url[1]
    res@metadata$header <- NULL
    res@metadata$file <- destfileRda

    ## See if we have an .rda file that is sufficiently youthful.
    if (file.exists(destfileRda)) {
        destfileAge <- (as.integer(Sys.time()) - as.integer(file.info(destfileRda)$mtime)) / 86400 # in days
        if (destfileAge < age) {
            argoFloatsDebug(debug, "The local .rda file\n    '", destfileRda, "'\n", sep="")
            argoFloatsDebug(debug, "is not being updated from\n    ", url[1], "\n", showTime=FALSE)
            argoFloatsDebug(debug, "because it is only", round(destfileAge, 4), "days old.\n", showTime=FALSE)
            argoFloatsDebug(debug, "About to load '", destfileRda, "'.\n", sep="")
            argoFloatsIndex <- NULL # defined again in next line; this is to quieten code-diagnostics
            load(destfileRda)
            argoFloatsDebug(debug, "Finished loading '", destfileRda, "'.\n", sep="")
            res@metadata$server <- server[1]
            res@metadata$file <- file
            res@metadata$destdir <- destdir
            res@metadata$destfileRda <- destfileRda
            res@metadata$ftpRoot <- argoFloatsIndex[["ftpRoot"]]
            res@metadata$header <- argoFloatsIndex[["header"]]
            res@data$index <- argoFloatsIndex[["index"]]
            argoFloatsDebug(debug, "} # getIndex()\n", style="bold", showTime=FALSE, unindent=1)
            return(res)
        }
    }
    ## We need to download data. We do that to a temporary file, because we will be saving
    ## an .rda file, not the data on the server.
    destfileTemp <- tempfile(pattern="argo", fileext=".gz")
    downloadSuccess <- FALSE
    for (iurl in seq_along(url)) {
        argoFloatsDebug(debug, "About to download temporary index file\n", sep="")
        argoFloatsDebug(debug, "    '", destfileTemp, "'\n", sep="", showTime=FALSE)
        argoFloatsDebug(debug, "from\n", sep="", showTime=FALSE)
        argoFloatsDebug(debug, "    '", url[1], "'\n", sep="", showTime=FALSE)
        status <- try(curl::curl_download(url=url[iurl], destfile=destfileTemp, quiet=quiet, mode="wb"), silent=TRUE)
        if (!inherits(status, "try-error")) {
            downloadSuccess <- TRUE
            break                      # the download worked
        }
        warning('Cannot download from server "', server, '".\n', immediate.=TRUE)
    }
    if (!downloadSuccess)
        stop("Could not download the file from any of these servers:\n'", paste(url, collapse="'\n'"), "'")
    argoFloatsDebug(debug, "About to read header.\n", sep="")
    first <- readLines(destfileTemp, 100)
    hash <- which(grepl("^#", first))
    ftpRoot <- gsub("^[^:]*:[ ]*(.*)$", "\\1", first[which(grepl("^# FTP", first))[1]])
    header <- first[hash]
    lastHash <- tail(hash, 1)
    names <- strsplit(first[1 + lastHash], ",")[[1]]
    if (grepl("merge", file)) {
        names <- c("file", "date", "latitude", "longitude", "ocean",
                   "profiler_type", "institution", "parameters",
                   "param_data_mode", "date_update")
        argoFloatsDebug(debug, "skipping (flawed) header in the merged file\n", sep="")
    }
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


#' Get Profiles Named in an argoFloats Index
#'
#' This takes an index constructed with [getIndex()], possibly
#' after focusing with [subset,argoFloats-method()], and creates
#' a list of files to download from the server named in the index.
#' Then these files are downloaded to the `destdir` directory,
#' using filenames inferred from the source filenames. The
#' value returned by [getProfiles()] is suitable for use
#' by [readProfiles()], and an example of this is given
#' in the documentation for the latter function.
#'
#' @param index an [argoFloats-class] object of type `"index"`, as created
#' by [getIndex()].
#' @param destdir character value naming the directory into which to store the
#' downloaded Argo files, or `NULL` (the default) to use the value of
#' `destdir` that was provided in the [getIndex()] call that created `index`.
#' @template force
#' @template retries
#' @template quiet
#' @template debug
#'
#' @return An object of class [argoFloats-class] with type=`"profiles"`, which
#' is suitable as the first argument of [readProfiles()].
#'
#' @examples
#' # Download some Argo data files.
#'\dontrun{
#' library(argoFloats)
#' data(index)
#' index2 <- subset(index, 1:2)
#' profiles2 <- getProfiles(index2, destdir=".")
#' # See ?readProfiles for how to read the files now downloaded.
#'}
#'
#' @author Dan Kelley
#'
#' @importFrom oce processingLogAppend vectorShow
#'
#' @export
getProfiles <- function(index, destdir=NULL, force=FALSE, retries=3, quiet=FALSE, debug=0)
{
    argoFloatsDebug(debug,  "getProfiles() {\n", style="bold", showTime=FALSE, unindent=1)
    if (missing(index))
        stop("In getProfiles() : must provide an index, as created by getIndex()", call.=FALSE)
    if (!inherits(index, "argoFloats"))
        stop("'index' must be an object created by getIndex()")
    res <- new("argoFloats", type="profiles")
    n <- length(index[["file"]])
    if (is.null(destdir))
        destdir <- index[["destdir"]]
    if (n == 0) {
        warning("In getProfiles() : the index has no files, so there is nothing to 'get'\n", call.=FALSE)
        file <- NULL
    } else {
        file <- rep("", n)
        argoFloatsDebug(debug, vectorShow(index[["ftpRoot"]]))
        argoFloatsDebug(debug, vectorShow(index[["file"]]))
        urls <- paste0(index[["ftpRoot"]], "/", index[["file"]])
        argoFloatsDebug(debug, oce::vectorShow(urls))
        for (i in seq_along(urls)) {
            file[i] <- getProfileFromUrl(urls[i], destdir=destdir,
                                         force=force, retries=retries, quiet=quiet, debug=debug)
        }
    }
    res@metadata$destdir <- destdir
    res@data$file <- file
    res@processingLog <- processingLogAppend(res@processingLog, "getProfiles(index, ...)")
    argoFloatsDebug(debug,  "} # getProfiles()\n", style="bold", showTime=FALSE, unindent=1)
    res
}
