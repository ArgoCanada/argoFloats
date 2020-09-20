## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Get Data for an Argo Float Profile
#'
#' @param url character value giving the URL for a netcdf file containing an
#' particular profile of a particular Argo float.
#' @template destdir
#'
#' @param destfile optional character value that specifies the name to be used
#' for the downloaded file. If this is not specified, then a name is determined
#' from the value of `url`.
#'
#' @template age
#'
#' @template retries
#'
#' @template quiet
#'
#' @template debug
#'
#' @return A character value naming the local location of the downloaded file,
#' or `NULL` if the file could not be downloaded.
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
#' if (requireNamespace("oce")) {
#'     dist <- oce::geodDist(index[["longitude"]], index[["latitude"]], lon0, lat0)
#'     w <- which.min(dist)
#'     url <- paste0(index[["metadata"]]["ftpRoot"], "/", index[["file"]][w])
#'     fileSable <- getProfileFromUrl(url=url)
#'     argoSable <- read.oce(fileSable)
#'     plot(argoSable, which=c(1, 4, 6, 5))
#' }
#'}
#'
#' @export
#'
#' @author Dan Kelley
getProfileFromUrl <- function(url=NULL, destdir="~/data/argo", destfile=NULL, age=365, retries=3, quiet=FALSE, debug=0)
{
    argoFloatsDebug(debug,  "getProfileFromUrl(url=\"", url, "\", destdir=\"", destdir, "\", destfile=\"",
                    if (missing(destfile)) "(missing)" else destfile, "\", ...) {", sep="", "\n", style="bold", unindent=1)
    if (length(url) != 1)
        stop("url must be of length 1, not of length ", length(url))
    ## If the ID starts with ftp://, then we just download the file directly, ignoring server
    if (!grepl("^ftp://", url))
        stop("the url must start with \"ftp://\" -- contact authors if you need this limitation to be lifted")
    if (is.null(destfile)) {
        destfile <- gsub(".*/(.*).nc", "\\1.nc", url)
        argoFloatsDebug(debug,  "inferred destfile=\"", destfile, "\" from url.\n", sep="")
    }
    res <- downloadWithRetries(url=url, destdir=destdir, destfile=destfile, mode="wb", quiet=quiet,
                               age=age, retries=retries, debug=debug-1)
    argoFloatsDebug(debug,  "} # getProfileFromUrl()", sep="", "\n", style="bold", unindent=1)
    if (is.na(res)) res else paste0(destdir, "/", destfile)
}


#' Get an Index of Available Argo Float Profiles
#'
#' This function gets an index of available Argo float profiles, typically
#' for later use as the first argument to [getProfiles()]. The work is done
#' either by downloading information from a data repository or by reusing an existing
#' index (packaged within an `.rda` file) that was prepared by a recent call to the
#' This choice is made behind the scenes, controlled by the `age` argument.
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
#' * `server`, the URL at which the index was found, and from
#'    which [getProfiles()] can construct URLs from which to
#'    download the netcdf files for individual float profiles.
#' * `filename`, the argument provided here.
#' * `header`, the preliminary lines in the source file that start
#'    with the `#` character.
#' * `data`, a data frame containing the items in the source file.
#'    The names of these items are determined automatically from
#'    `"argo"` and `"bgcargo"` files, but for `"merged"` files,
#'    the header is malformed (as of February 2020) and so the names
#'    are set based on the authors' inspection of a downloaded file.
#'
#' Some expertise is required in deciding on the value for the
#' `file` argument to [getIndex()].  As of June 2020, the
#' FTP sites
#' \url{ftp://usgodae.org/pub/outgoing/argo}
#' and
#' \url{ftp://ftp.ifremer.fr/ifremer/argo}
#' contain multiple index files, as listed in the left-hand column of the
#' following table. The middle column lists nicknames
#' for some of the files.  These can be provided as the `file` argument,
#' as alternatives to the full names.
#' The right-hand column describes the file contents.
#' Note that the servers also provide files with names similar to those
#' given in the table, but ending in `.txt`.  These are uncompressed
#' equivalents of the `.gz` files that offer no advantage and take
#' longer to download, so [getIndex()] is not designed to work with them.
#' Finally, note that, as of June 2020,
#' the usgodae server does not supply `"synthetic"` files, but
#' the ifremer server does; this is typically not a concern to users,
#' because `getIndex` searches both servers for index files.
#' \tabular{lll}{
#' *File Name*                           \tab *Nickname*              \tab *Contents*\cr
#' `ar_greylist.txt`                     \tab -                       \tab Suspicious/malfunctioning floats\cr
#' `ar_index_global_meta.txt.gz`         \tab -                       \tab Metadata files\cr
#' `ar_index_global_prof.txt.gz`         \tab `"argo"`                \tab Argo data\cr
#' `ar_index_global_tech.txt.gz`         \tab -                       \tab Technical files\cr
#' `ar_index_global_traj.txt.gz`         \tab -                       \tab Trajectory files\cr
#' `argo_bio-profile_index.txt.gz`       \tab `"bgc"` or `"bgcargo"`  \tab Biogeochemical Argo data (without S or T)\cr
#' `argo_bio-traj_index.txt.gz`          \tab -                       \tab Bio-trajectory files\cr
#' `argo_merge-profile_index.txt.gz`     \tab `"merge"` or `"merged"` \tab Merged `"argo"` and `"bgc"` data\cr
#' `argo_synthetic-profile_index.txt.gz` \tab `"synthetic"`           \tab Synthetic data, successor to `"merge"`\cr
#' }
#'
#' The next step after using [getIndex()] is usually to
#' use [getProfiles()], which downloads or checks for local
#' copies of the per-profile data files that are listed in an
#' index, and this is typically followed by a call to
#' [readProfiles()], which reads the local files, yielding
#' an object that can be plotted or analysed in other ways.
#'
#' @param filename character value that indicates the file name on the server, as in
#' the first column of the table given in \dQuote{Details}, or (for some file types)
#' as in the nickname given in the middle column. Note that the downloaded
#' file name will be based on the full file name given as this argument, and
#' that nicknames are expanded to the full filenames before saving.
#'
#' @param server character value, or vector of character values, indicating
#' the name of servers that supply argo data.  If more than
#' one value is given, then these are tried sequentially until one
#' is found to supply the index file named in the `filename` argument.
#' As of May 2020, the two servers known to work are
#' `"ftp://usgodae.org/pub/outgoing/argo"` and
#' `"ftp://ftp.ifremer.fr/ifremer/argo"`.  These may be referred
#' to with nicknames `"usgodae"` and `"ifremer"`.  As a further
#' convenience, a third nickname (and the default for this argument)
#' is also available: `"auto"` is expanded to `c("usgodae","ifremer")`.
#' Note that if a nickname is not used, the character value(s) in `server`
#' must start with `"ftp://"`.
#'
#' @template destdir
#'
#' @param age numeric value indicating how old a downloaded file
#' must be (in days), for it to be considered out-of-date.  The
#' default, `age=1`, limits downloads to once per day, as a way
#' to avoid slowing down a workflow with a download that might take
#' a minute or so. Note that setting `age=0` will force a new
#' download, regardless of the age of the local file, and that
#' age is changed to 0 if `keep` is `TRUE`.
#'
#' @param quiet logical value indicating whether to silence some
#' progress indicators.  The default is to show such indicators.
#'
#' @param keep logical value indicating whether to retain the
#' raw index file as downloaded from the server.  This is `FALSE`
#' by default, indicating that the raw index file is to be
#' discarded once it has been analysed.  Note that if `keep`
#' is `TRUE`, then the supplied value of `age` is converted
#' to 0, to force a new download.
#'
#' @template debug
#'
#' @return An object of class [`argoFloats-class`] with type=`"index"`, which
#' is suitable as the first argument of [getProfiles()].
#'
#' @examples
#'\dontrun{
#' # Download an index of merged argo/bgc-argo floats, and plot temporal coverage.
#' library(argoFloats)
#' i <- getIndex("merged")
#' summary(i)
#' hist(i[["date"]], breaks="years", main="", xlab="Time", freq=TRUE)}
#'
#' @author Dan Kelley
#'
#' @importFrom utils read.csv tail
## @importFrom curl curl_download
## @importFrom oce processingLogAppend
#' @export
getIndex <- function(filename="argo",
                     server="auto",
                     destdir="~/data/argo",
                     age=1,
                     quiet=FALSE,
                     keep=FALSE,
                     debug=0)
{
    if (!requireNamespace("oce", quietly=TRUE))
        stop("must install.packages(\"oce\"), for getIndex() to work")
    if (!requireNamespace("curl", quietly=TRUE))
        stop("must install.packages(\"curl\") for getIndex() to work")
    if (!is.logical(quiet))
        stop("quiet must be a logical value")
    if (1 != length(quiet))
        stop("quiet must be a single value")
    if (!is.logical(keep))
        stop("keep must be a logical value")
    if (1 != length(keep))
        stop("keep must be a single value")
    if (keep)
        age <- 0
    ## Sample file
    ## ftp://ftp.ifremer.fr/ifremer/argo/dac/aoml/1900710/1900710_prof.nc
    ## ftp://usgodae.org/pub/outgoing/argo/dac/aoml/1900710/1900710_prof.nc
    res <- new("argoFloats", type="index")
    res@metadata$destdir <- destdir
    argoFloatsDebug(debug,  "getIndex(server='", server, "', filename='", filename, "'", ", destdir='", destdir, "') {", sep="", "\n", style="bold", showTime=FALSE, unindent=1)
    serverOrig <- server
    if (length(server) == 1 && server == "auto") {
        server <- c("usgodae", "ifremer")
        argoFloatsDebug(debug, "Server 'auto' expanded to c('",
                        paste(server, collapse="', '"), '").\n', sep="")
    }
    for (iserver in seq_along(server)) {
        if (server[iserver] == "usgodae") {
            server[iserver] <- "ftp://usgodae.org/pub/outgoing/argo"
            argoFloatsDebug(debug, "Server 'usgodae' expanded to '", server[iserver], "'.\n", sep="")
        } else if (server[iserver] == "ifremer") {
            server[iserver] <- "ftp://ftp.ifremer.fr/ifremer/argo"
            argoFloatsDebug(debug, "Server 'ifremer' expanded to '", server[iserver], "'.\n", sep="")
        }
    }

    if (!all(grepl("^ftp://", server)))
        stop("server must be \"auto\", \"usgodae\", \"ifremer\", or a vector of strings starting with \"ftp://\", but it is ",
             if (length(server) > 1) paste0("\"", paste(server, collapse="\", \""), "\"")
             else paste0("\"", server, "\""), "\n", sep="")
    ## Ensure that we can save the file
    if (!file.exists(destdir))
        stop("First, create a directory named '", destdir, "'")
    if (!file.info(destdir)$isdir)
        stop("'", destdir, "' is not a directory")
    ## Handle nicknames
    filenameOrig <- filename
    if (filename == "argo") {
        filename <- "ar_index_global_prof.txt.gz"
    } else if (filename == "bgcargo" || filename == "bgc") {
        filename <- "argo_bio-profile_index.txt.gz"
    } else if (filename == "merge" || filename == "merged") {
        filename <- "argo_merge-profile_index.txt.gz"
    } else if (filename == "synthetic") {
        filename <- "argo_synthetic-profile_index.txt.gz"
    }
    if (filename != filenameOrig)
        argoFloatsDebug(debug, "Converted filename='", filenameOrig, "' to filename='", filename, "'.\n", sep="")
    ## Note: 'url' is a vector; e.g. using server="auto" creates 2 elements in url
    url <- paste(server, filename, sep="/")
    destfile <- paste(destdir, filename, sep="/")
    ## NOTE: we save an .rda file, not the .gz file, for speed of later operations
    if (grepl("\\.txt\\.gz$", destfile)) {
        destfileRda <- gsub(".txt.gz$", ".rda", destfile)
    } else if (grepl("\\.txt", destfile)) {
        destfileRda <- gsub(".txt$", ".rda", destfile)
    } else {
        stop("cannot construct .rda filename (please report an issue)")
    }
    argoFloatsDebug(debug, "Set destfileRda='", destfileRda, "'.\n", sep="")
    res@metadata$url <- url[1]
    res@metadata$header <- NULL
    #res@metadata$filename <- destfileRda

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
            #res@metadata$filename <- filename
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
    argoFloatsDebug(debug, "Allocated the temporary file\n    '", destfileTemp, "'.\n", sep="")
    failedDownloads <- 0
    iurlSuccess <- 0                   # set to a positive integer in the following loop, if we succeed
    for (iurl in seq_along(url)) {
        argoFloatsDebug(debug, "About to try downloading index file from\n    '", url[iurl], "'.\n", sep="")
        status <- try(curl::curl_download(url=url[iurl],
                                          destfile=destfileTemp,
                                          quiet=quiet,
                                          mode="wb"))
        ## status <- capture.output(try(curl::curl_download(url=url[iurl],
        ##                                                  destfile=destfileTemp,
        ##                                                  quiet=quiet,
        ##                                                  mode="wb"),
        ##                              silent=!TRUE),
        ##                          type="message")
        if (!inherits(status, "try-error")) {
            if (failedDownloads > 0)
                message("Downloaded index from ", server[iurl])
            iurlSuccess <- iurl
            break                      # the download worked
        }
        if (iurl == length(url))
            message("Can't download index from ", server[iurl])
        else
            message("Can't download index from ", server[iurl], ", so moving to next server")
        failedDownloads <- failedDownloads + 1
    }
    if (0 == iurlSuccess)
        stop("Could not download index from any of these servers:\n'", paste(url, collapse="'\n'"), "'")
    argoFloatsDebug(debug, "About to read the header at the start of the index file.\n", sep="")
    first <- readLines(destfileTemp, 100)
    ## Typically, length(ftpRoot) is 2
    ftpRoot <- gsub("^[^:]*:[ ]*(.*)$", "\\1", first[which(grepl("^# FTP", first))])
    hash <- which(grepl("^#", first))
    header <- first[hash]
    lastHash <- tail(hash, 1)
    names <- strsplit(first[1 + lastHash], ",")[[1]]
    if (grepl("merge", filename)) {
        names <- c("file", "date", "latitude", "longitude", "ocean",
                   "profiler_type", "institution", "parameters",
                   "param_data_mode", "date_update")
        argoFloatsDebug(debug, "Skipping (flawed) header in the merged file.\n", sep="")
    }
    argoFloatsDebug(debug, "Reading index file contents (can be slow).\n", sep="")
    index <- read.csv(destfileTemp, skip=2 + lastHash, col.names=names, stringsAsFactors=FALSE)
    argoFloatsDebug(debug, "Setting out-of-range latitude and longitude to NA.\n", sep="")
    if ("latitude" %in% names(index)) {
        index$latitude[abs(index$latitude) > 90] <- NA
    } else {
        stop("this index file is mis-configured; it does not contain a column named \"latitude\"")
    }
    if ("longitude" %in% names(index)) {
        index$longitude[abs(index$longitude) > 360] <- NA
    } else {
        stop("this index file is mis-configured; it does not contain a column named \"longitude\"")
    }
    argoFloatsDebug(debug, "Decoding dates.\n", sep="")
    index$date <- as.POSIXct(as.character(index$date), format="%Y%m%d%H%M%S", tz="UTC")
    index$date_update <- as.POSIXct(as.character(index$date_update), format="%Y%m%d%H%M%S",tz="UTC")
    argoFloatsIndex <- list(server=server[iurlSuccess], ftpRoot=ftpRoot, header=header, index=index)
    save(argoFloatsIndex, file=destfileRda)
    if (keep) {
        to <- paste0(destdir, "/", gsub(".*/", "", url[iurlSuccess]))
        argoFloatsDebug(debug, "Storing temporary raw index file\n    '", destfileTemp, "'\n  locally as\n    '", to, "'.\n", sep="")
        file.copy(from=destfileTemp, to=to)
    }
    argoFloatsDebug(debug,  "Removing temporary file\n    '", destfileTemp, "'.\n", sep="")
    unlink(destfileTemp)
    res@metadata$server <- server[iurlSuccess]
    res@metadata$url <- url[iurlSuccess]
    #res@metadata$filename <- filename[iurlSuccess]
    res@metadata$destfileRda <- destfileRda
    res@metadata$ftpRoot <- argoFloatsIndex$ftpRoot
    res@metadata$header <- argoFloatsIndex$header
    res@data$index <- argoFloatsIndex$index
    res@processingLog <- oce::processingLogAppend(res@processingLog,
                                                  paste("getIndex(server=",
                                                        if (length(serverOrig) == 1) paste("\"", serverOrig, "\", ", sep="")
                                                        else paste("c(\"", paste(serverOrig, collapse="\", \""), "\"), ", sep=""),
                                                        "filename=\'", filename, "\", age=", age, ")", sep=""))
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
#' by [readProfiles()].
#'
#' It should be noted that the constructed server URL follows
#' a different pattern on the usgodae an ifremer servers, and
#' so if some other server is used, the URL may be wrong, leading
#' to an error.  Similarly, if the patterns on these two
#' servers change, then [getProfiles()] will fail. Users who
#' encounter such problems are requested to report them
#' to the authors.
#'
#' If a particular data file cannot be downloaded after multiple trials, then
#' the behaviour depends on the value of the `skip` argument.  If that is
#' `TRUE` then a `NA` value is inserted in the corresponding
#' spot in the return value, but if it is `FALSE` (the default), then an error is reported.
#' Note that [readProfiles()] skips over any such `NA` entries,
#' while reporting their positions within `index`.
#'
#' @param index an [`argoFloats-class`] object of type `"index"`, as created
#' by [getIndex()].
#'
#' @param destdir character value naming the directory into which to store the
#' downloaded Argo files, or `NULL` (the default) to use the value of
#' `destdir` that was provided in the [getIndex()] call that created `index`.
#'
#' @template age
#'
#' @template retries
#'
#' @template skip
#'
#' @template quiet
#'
#' @template debug
#'
#' @return An object of class [`argoFloats-class`] with type=`"profiles"`, the
#' `data` slot of which contains two items: `url`,
#' which holds the URLs from which the netcdf
#' files were downloaded, and `file`, which
#' holds the path names of the downloaded files; the latter
#' is used by [readProfiles()].
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
## @importFrom oce processingLogAppend vectorShow
#'
#' @export
getProfiles <- function(index, destdir=NULL, age=365, retries=3, skip=TRUE, quiet=TRUE, debug=0)
{
    debug <- max(0, min(3, floor(debug+0.5)))
    if (!requireNamespace("oce", quietly=TRUE))
        stop("must install.packages(\"oce\") for getProfiles() to work")
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
        argoFloatsDebug(debug, oce::vectorShow(index[["ftpRoot"]]))
        argoFloatsDebug(debug, oce::vectorShow(index[["file"]]))
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
        ## I *thought* the USGODAE and IFREMER servers were once set up differently, with only usgodae having "dac" in the path
        ## name.  That is why the next block was written.  However, as of May 15, 2020, it seems they are set up in the same
        ## way, so the ifremer case was rewritten to match the usgodae case.  Still, I am keeping this if block, in case I am in
        ## error.  Note also that we have a place another server type, and it defaults to no "dac" ... but since I have never
        ## seen a third type, I imagine that part has never been exectuted.
        if (grepl("usgodae.org", server, ignore.case=TRUE)) {
            urls <- paste0(server, "/dac/", index[["file"]])
        } else if (grepl("ifremer.fr", server, ignore.case=TRUE)) {
            urls <- paste0(server, "/dac/", index[["file"]])
        } else {
            urls <- paste0(server, "/", index[["file"]])
            warning("guessing on URL form (e.g. \"", urls[1], "\"), because server is neither usgodae.org nor ifremer.fr\n", immediate.=TRUE)
        }
        argoFloatsDebug(debug, oce::vectorShow(urls))
        file <- vector("character", length(urls))
        for (i in seq_along(urls)) {
            name <- getProfileFromUrl(urls[i], destdir=destdir, age=age, retries=retries, quiet=quiet, debug=debug-1)
            if (is.na(name) && !skip)
                stop("cannot download file '", urls[i], "', which is considered a failure, since skip=FALSE.  This normally results from a stale index file; try using getIndex(age=0) to refresh your index.")
            file[i] <- name
        }
    }
    res@metadata$destdir <- destdir
    res@data$url <- urls
    res@data$file <- file
    res@processingLog <- oce::processingLogAppend(res@processingLog, "getProfiles(index, ...)")
    argoFloatsDebug(debug,  "} # getProfiles()\n", style="bold", showTime=FALSE, unindent=1)
    res
}
