## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

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
#' index <- getIndex()
#' lon0 <- -59.9149
#' lat0 <- 43.9337
#' dist <- geodDist(index[["longitude"]], index[["latitude"]], lon0, lat0)
#' w <- which.min(dist)
#' url <- paste0(index[["metadata"]]["ftpRoot"], "/", index[["file"]][w])
#' fileSable <- getProfileFromUrl(url=url)
#' argoSable <- read.oce(fileSable)
#' plot(argoSable, which=c(1, 4, 6, 5))
#'}
#'
#' @author Dan Kelley
#'
#' @importFrom curl curl_download
#' @export
getProfileFromUrl <- function(url=NULL, destdir="~/data/argo", destfile=NULL,
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
#' This function gets an index of available Argo float profiles, either
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
#' @param server character value, or vector of character values, giving
#' the base name(s) of server(s) holding argo profile files.  These servers
#' are tried sequentially until one of them works.  The default
#' value of `server` is `"auto"`, which is automatically
#' expanded to
#' `c("ftp://usgodae.org/pub/outgoing/argo", "ftp://ftp.ifremer.fr/ifremer/argo")`,
#' meaning to try the USGODAE server first, but to switch to the IFREMER
#' server if that fails. Unless `server` is `auto`, the first 5 characters of
#' `server` must be `"ftp://"`, as a way to catch errors.
#'
#' @param filename character value that indicates the file name on the server, as in
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
#' @return An object of class [`argoFloats-class`] with type=`"index"`, which
#' is suitable as the first argument of [getProfiles()].
#'
#' @examples
#'\dontrun{
#' library(argoFloats)
#' library(oce)
#' # Example: Temporal and spatial coverage of merged argo/bgcargo measurements.
#' par(mfrow=c(2, 1), mar=c(3, 3, 1, 1))
#' ai <- getIndex(filename="merged")
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
                     filename="argo",
                     destdir="~/data/argo",
                     age=7,
                     quiet=FALSE,
                     debug=0)
{
    ## Sample file
    ## ftp://ftp.ifremer.fr/ifremer/argo/dac/aoml/1900710/1900710_prof.nc
    ## ftp://usgodae.org/pub/outgoing/argo/dac/aoml/1900710/1900710_prof.nc
    res <- new("argoFloats", type="index")
    res@metadata$destdir <- destdir
    argoFloatsDebug(debug,  "getIndex(server=\"", server, "\", filename=\"", filename, "\"", ", destdir=\"", destdir, "\") {", sep="", "\n", style="bold", showTime=FALSE, unindent=1)
    if (length(server) == 1 && server == "auto") {
        server <- c("ftp://usgodae.org/pub/outgoing/argo",
                    "ftp://ftp.ifremer.fr/ifremer/argo")
        argoFloatsDebug(debug, 'server "auto" expanded to c("',
                        paste(server, collapse='", "'), "')\n", sep="")
    }
    if (!all(grepl("^ftp://", server)))
        stop("server must be 'auto', or a vector of strings starting with \"ftp://\", but it is ",
             if (length(server) > 1) paste0("\"", paste(server, collapse="\", \""), "\"")
             else paste0("\"", server, "\""), "\n", sep="")
    ## Ensure that we can save the file
    if (!file.exists(destdir))
        stop("First, create a directory named '", destdir, "'")
    if (!file.info(destdir)$isdir)
        stop("'", destdir, "' is not a directory")
    ## Handle nicknames
    if (filename == "argo") {
        filename <- "ar_index_global_prof.txt.gz"
        argoFloatsDebug(debug, "Converted filename=\"argo\" to filename=\"", filename, "\".\n", sep="")
    } else if (filename == "bgcargo" || filename == "bgc") {
        filename <- "argo_bio-profile_index.txt.gz"
        argoFloatsDebug(debug, "Converted filename=\"bgcargo\" to filename=\"", filename, "\".\n", sep="")
    } else if (filename == "merge" || filename == "merged") {
        filename <- "argo_merge-profile_index.txt.gz"
        argoFloatsDebug(debug, "Converted filename=\"argo_merge\" to filename=\"", filename, "\".\n", sep="")
    }
    ## Note: 'url' is a vector; e.g. using server="auto" creates 2 elements in url
    url <- paste(server, filename, sep="/")
    destfile <- paste(destdir, filename, sep="/")
    ## NOTE: we save an .rda file, not the .gz file, for speed of later operations
    destfileRda <- gsub(".gz$", ".rda", destfile)
    destfileRda <- gsub(".txt$", ".rda", destfile)
    res@metadata$url <- url[1]
    res@metadata$header <- NULL
    res@metadata$filename <- destfileRda

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
            res@metadata$filename <- filename
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
        status <- try(curl::curl_download(url=url[iurl], destfile=destfileTemp, quiet=quiet, mode="wb"), silent=!TRUE)
        if (!inherits(status, "try-error")) {
            server <- server[iurl]
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
    ## Typically, length(ftpRoot) is 2
    ftpRoot <- gsub("^[^:]*:[ ]*(.*)$", "\\1", first[which(grepl("^# FTP", first))])
    header <- first[hash]
    lastHash <- tail(hash, 1)
    names <- strsplit(first[1 + lastHash], ",")[[1]]
    if (grepl("merge", filename)) {
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
    argoFloatsIndex <- list(ftpRoot=ftpRoot, server=server, filename=filename, header=header, index=index)
    save(argoFloatsIndex, file=destfileRda)
    argoFloatsDebug(debug,  "removing temporary file '", destfileTemp, "'.\n", sep="")
    unlink(destfileTemp)
    res@metadata$server <- server
    res@metadata$filename <- filename
    res@metadata$destfileRda <- destfileRda
    res@metadata$server <- argoFloatsIndex$server
    res@metadata$ftpRoot <- argoFloatsIndex$ftpRoot
    res@metadata$header <- argoFloatsIndex$header
    res@data$index <- argoFloatsIndex$index
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("getIndex(server='", server, "', filename='", filename, "', age=", age, ")", sep=""))
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
#' in the documentation for [readProfiles()].
#'
#' It should be noted that the constructed server URL follows
#' a different pattern on the usgodae an ifremer servers, and
#' so if some other server is used, the URL may be wrong, leading
#' to an error.  Similarly, if the patterns on these two
#' servers change, then [getProfiles()] will fail. Users who
#' encounter such problems are requested to report them
#' to the authors; in a pinch, they may try altering the
#' conditional block that follows the line
#' ```
#' ## NB. the USGODAE and IFREMER servers are set up differently.
#' ```
#' in the source-code file named `R/get.R`.
#'
#' If the data file cannot be downloaded after multiple trials, an
#' error is issued, with a hint that running [getIndex()] again
#' might help, in case the filename on the server has changed since
#' the index was last downloaded by [getIndex()].
#'
#'
#' @param index an [`argoFloats-class`] object of type `"index"`, as created
#' by [getIndex()].
#' @param destdir character value naming the directory into which to store the
#' downloaded Argo files, or `NULL` (the default) to use the value of
#' `destdir` that was provided in the [getIndex()] call that created `index`.
#' @template force
#' @template retries
#' @template quiet
#' @template debug
#'
#' @return An object of class [`argoFloats-class`] with type=`"profiles"`, which
#' is suitable as the first argument of [readProfiles()].
#'
#' @examples
#' # Download some Argo data files.
#'\dontrun{
#' library(argoFloats)
#' data(index)
#' index2 <- subset(index, 1:2)
#' profiles2 <- getProfiles(index2)
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
        ## The Construction of the remote filename is tricky was changed on 2020-04-30. Previously,
        ## we used "ftpRoot", inferred from the "# FTP" line in the header in the index file.
        ## However, as discussed at https://github.com/ArgoCanada/argoFloats/issues/82,
        ## this decision was faulty, in the sense that it sometimes worked and sometimes
        ## failed, for the *same* index object -- that is, the file structure on the server
        ## must be changeable.  So, as a test (which will need to be reevaluated over time),
        ## we switched to using "server" instead of "ftpRoot".  That means that we seek
        ## the netcdf files from the same source as the index, and *not* from the source listed
        ## in the "# FTP" line in the header within that index.
        server <- index[["server"]]
        ## NB. the USGODAE and IFREMER servers are set up differently.
        urls <- if (grep("usgodae.org", server, ignore.case=TRUE)) {
            urls <- paste0(server, "/dac/", index[["file"]])
        } else if (grep("ifremer.fr", server, ignore.case=TRUE)) {
            urls <- paste0(server, "/", index[["file"]])
        } else {
            urls <- paste0(server, "/", index[["file"]])
            warning("guessing on URL form (e.g. \"", urls[1], "\"), because server is neither usgodae.org nor ifremer.fr\n", immediate.=TRUE)
        }
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
