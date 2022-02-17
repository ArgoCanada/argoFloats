# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

argoFloatsCacheEnv <- new.env(parent=emptyenv())

#' Check Whether an Item is Cached
#'
#' @param name character value, naming the item.
#'
#' @param debug an integer, passed to [argoFloatsDebug()].
#'
#' @export
#'
#' @return A logical value indicating whether a cached value is available.
#'
#' @author Dan Kelley, making a thin copy of code written by Dewey Dunnington
argoFloatsIsCached <- function(name, debug=0)
{
    argoFloatsDebug(debug, "in argoFloatsIsCached()\n")
    name %in% names(argoFloatsCacheEnv)
}

#' Get an Item From The Cache
#'
#' @param name character value, naming the item.
#'
#' @param debug an integer, passed to [argoFloatsDebug()].
#'
#' @return The cached value, as stored with [argoFloatsStoreInCache()].
#'
#' @export
argoFloatsGetFromCache <- function(name, debug=0)
{
    argoFloatsDebug(debug, "in argoFloatsGetFromCache()\n")
    argoFloatsCacheEnv[[name]]
}

#' Store an Item in the Cache
#'
#' @param name character value, naming the item.
#'
#' @param value the new contents of the item.
#'
#' @param debug an integer, passed to [argoFloatsDebug()].
#'
#' @return None (invisible NULL).
#'
#' @export
argoFloatsStoreInCache <- function(name, value, debug=0)
{
    argoFloatsDebug(debug, "in argoFloatsStoreInCache()\n")
    argoFloatsCacheEnv[[name]] <- value
    invisible(NULL)
}

#' Get Data for an Argo Float Profile
#'
#' @param url character value giving the URL for a NetCDF file containing an
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
## NOTE: this will take longer than 5s, so I had marked it dontrun on an initial
## CRAN submission.  THe CRAN review suggested marking it donttest instead, but
## when I do that, rhub::check_for_cran() gives an error on the getIndex, relating
## to the non-existence of ~/data/argo.  But, really, what's the point of these
## examples?  The docs are really quite straightforward, and we have a paper,
## besides. I don't see that these examples illustrate anything that the docs
## don't already illustrate, and so I have removed them. --DK
## @examples
##\dontrun{
## # This is not run, because it takes longer than 5s to complete.
## # These examples assume that the ~/data/argo directory exists.
## library(argoFloats)
## library(oce)
##
## # Example 1: a particular file
## url <- "ftp://ftp.ifremer.fr/ifremer/argo/dac/nmdis/2901633/profiles/R2901633_071.nc"
## file <- getProfileFromUrl(url=url)
## argo <- read.argo(file)
## # Note that oce::plot() changes par(mfrow) and par(mar).
## oce::plot(argo, which=c(1, 4, 6, 5))
##
## # Example 2: argo profile nearest Sable Island
## index <- getIndex()
## lon0 <- -59.9149
## lat0 <- 43.9337
##
## dist <- oce::geodDist(index[["longitude"]], index[["latitude"]], lon0, lat0)
## w <- which.min(dist)
## url <- paste0(index[["metadata"]][["ftpRoot"]][1], "/", index[["file"]][w])
## fileSable <- getProfileFromUrl(url=url)
## argoSable <- read.oce(fileSable)
## # Note that oce::plot() changes par(mfrow) and par(mar).
## plot(argoSable, which=c(1, 4, 6, 5))
##}
#'
#' @export
#'
#' @author Dan Kelley
getProfileFromUrl <- function(url=NULL, destdir=argoDefaultDestdir(), destfile=NULL, age=argoDefaultProfileAge(), retries=3, quiet=FALSE, debug=0)
{
    argoFloatsDebug(debug,  "getProfileFromUrl(url=\"", url, "\", destdir=\"", destdir, "\", destfile=\"",
                    if (missing(destfile)) "(missing)" else destfile, "\", ...) {", sep="", "\n", style="bold", unindent=1)
    if (length(url) != 1)
        stop("url must be of length 1, not of length ", length(url))
    if (is.null(destfile)) {
        destfile <- gsub(".*/(.*).nc", "\\1.nc", url)
        argoFloatsDebug(debug,  "inferred destfile=\"", destfile, "\" from url.\n", sep="")
    }
    res <- downloadWithRetries(url=url, destdir=destdir, destfile=destfile, quiet=quiet,
                               age=age, retries=retries, debug=debug-1)
    argoFloatsDebug(debug,  "} # getProfileFromUrl()", sep="", "\n", style="bold", unindent=1)
    if (is.na(res)) res else paste0(destdir, "/", destfile)
}


#' Get an Index of Available Argo Float Profiles
#'
#' This function gets an index of available Argo float profiles, typically
#' for later use as the first argument to [getProfiles()]. The work is done
#' either by downloading information from a data repository or by reusing an existing
#' index (packaged within an `.rda` file) that is controlled by the `age` argument behind the scenes.
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
#'    download the NetCDF files for individual float profiles.
#' * `filename`, the argument provided here.
#' * `header`, the preliminary lines in the source file that start
#'    with the `#` character.
#' * `data`, a data frame containing the items in the source file.
#'    The names of these items are determined automatically from
#'    `"core"`,`"bgcargo"`, `"synthetic"` files.
#'
#' Some expertise is required in deciding on the value for the
#' `file` argument to [getIndex()].  As of June 2020, the
#' FTP sites
#' `ftp://usgodae.org/pub/outgoing/argo`
#' and
#' `ftp://ftp.ifremer.fr/ifremer/argo`
#' contain multiple index files, as listed in the left-hand column of the
#' following table. The middle column lists nicknames
#' for some of the files.  These can be provided as the `file` argument,
#' as alternatives to the full names.
#' The right-hand column describes the file contents.
#' Note that the servers also provide files with names similar to those
#' given in the table, but ending in `.txt`.  These are uncompressed
#' equivalents of the `.gz` files that offer no advantage and take
#' longer to download, so [getIndex()] is not designed to work with them.
#' \tabular{lll}{
#' *File Name*                           \tab *Nickname*              \tab *Contents*\cr
#' `ar_greylist.txt`                     \tab -                       \tab Suspicious/malfunctioning floats\cr
#' `ar_index_global_meta.txt.gz`         \tab -                       \tab Metadata files\cr
#' `ar_index_global_prof.txt.gz`         \tab `"argo"` or `"core"`    \tab Argo data\cr
#' `ar_index_global_tech.txt.gz`         \tab -                       \tab Technical files\cr
#' `ar_index_global_traj.txt.gz`         \tab `"traj"`                \tab Trajectory files\cr
#' `argo_bio-profile_index.txt.gz`       \tab `"bgc"` or `"bgcargo"`  \tab Biogeochemical Argo data (without S or T)\cr
#' `argo_bio-traj_index.txt.gz`          \tab `"bio-traj"`            \tab Bio-trajectory files\cr
#' `argo_synthetic-profile_index.txt.gz` \tab `"synthetic"`           \tab Synthetic data, successor to `"merge"`\cr
#' }
#' Note: as of Dec 01,2020 the user will no longer have the option to use `"argo"` as a filename argument. Instead, `"core"` will
#' be used.
#'
#' The next step after using [getIndex()] is usually to
#' use [getProfiles()], which downloads or checks for local
#' copies of the per-profile data files that are listed in an
#' index, and this is typically followed by a call to
#' [readProfiles()], which reads the local files, yielding
#' an object that can be plotted or analyzed in other ways.
#' For more on this function, see section 2 of Kelley et al. (2021).
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
#' As of December 2020, the three servers known to work are
#' `"https://data-argo.ifremer.fr"`, `"ftp://ftp.ifremer.fr/ifremer/argo"` and
#' `"ftp://usgodae.org/pub/outgoing/argo"`.
#' These may be referred
#' to with nicknames `"ifremer-https"`, `"ifremer"`and  `"usgodae"`.
#' Any URL that can be used in [curl::curl_download()] is a valid value provided
#' that the file structure is identical to the mirrors listed above. See
#' [argoDefaultServer()] for how to provide a default value.
#'
#' @template destdir
#'
#' @param age numeric value indicating how old a downloaded file
#' must be (in days), for it to be considered out-of-date.  The
#' default, [argoDefaultIndexAge()], limits downloads to once per day, as a way
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
#' discarded once it has been analyzed  Note that if `keep`
#' is `TRUE`, then the supplied value of `age` is converted
#' to 0, to force a new download.
#'
#' @template debug
#'
#' @return An object of class [`argoFloats-class`] with type=`"index"`, which
#' is suitable as the first argument of [getProfiles()].
#'
## NOTE: this will take longer than 5s, so I had marked it dontrun on an initial
## CRAN submission.  THe CRAN review suggested marking it donttest instead, but
## when I do that, rhub::check_for_cran() gives an error on the getIndex, relating
## to the non-existence of ~/data/argo.  But, really, what's the point of these
## examples?  The docs are really quite straightforward, and we have a paper,
## besides. I don't see that these examples illustrate anything that the docs
## don't already illustrate, and so I have removed them. --DK
## @examples
##\dontrun{
## # This is not run, because it takes longer than 5s to complete.
## # Download an index of synthetic Argo/BGC-Argo floats, and plot temporal coverage.
## library(argoFloats)
## i <- getIndex("synthetic")
## summary(i)
## hist(i[["date"]], breaks="years", main="", xlab="Time", freq=TRUE)
##}
#'
#' @references
#' Kelley, D. E., Harbin, J., & Richards, C. (2021). argoFloats: An R package for analyzing
#' Argo data. Frontiers in Marine Science, (8), 636922.
#' \doi{10.3389/fmars.2021.635922}
#'
#' @author Dan Kelley
#'
#' @importFrom utils read.csv tail
## @importFrom lubridate fast_strptime
## @importFrom curl curl_download
## @importFrom oce processingLogAppend
#' @export
getIndex <- function(filename="core",
                     server=argoDefaultServer(),
                     destdir=argoDefaultDestdir(),
                     age=argoDefaultIndexAge(),
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
    istraj <- filename %in% c("traj", "bio-traj", "ar_index_global_traj.txt.gz", "argo_bio-traj_index.txt.gz")
    res <- new("argoFloats", type="index", subtype=if (istraj) "trajectories" else "cycles")
    argoFloatsDebug(debug,  "getIndex(server='", server, "', filename='", filename, "'", ", destdir='", destdir, "') {", sep="", "\n", style="bold", showTime=FALSE, unindent=1)
    serverOrig <- server
    serverNicknames <- c("ifremer-https" = "https://data-argo.ifremer.fr",
                         "ifremer" = "ftp://ftp.ifremer.fr/ifremer/argo",
                         "usgodae" = "ftp://usgodae.org/pub/outgoing/argo")
    serverIsNickname <- server %in% names(serverNicknames)
    server[serverIsNickname] <- serverNicknames[server[serverIsNickname]]

    if (!all(grepl("^[a-z]+://", server)))
        stop("server must be \"ifremer-https\", \"usgodae\", \"ifremer\", or a vector of urls, but it is ",
             if (length(server) > 1) paste0("\"", paste(server, collapse="\", \""), "\"")
             else paste0("\"", server, "\""), "\n", sep="")
    ## Ensure that we can save the file
    if (!file.exists(destdir))
        stop("First, create a directory named '", destdir, "'")
    if (!file.info(destdir)$isdir)
        stop("'", destdir, "' is not a directory")
    ## Handle nicknames
    filenameOrig <- filename
    names <- c("core","bgc","bgcargo","synthetic", "traj", "bio-traj", "ar_index_global_prof.txt.gz", "argo_bio-profile_index.txt.gz", "argo_synthetic-profile_index.txt.gz", "ar_index_global_traj.txt.gz", "argo_bio-traj_index.txt.gz")
    if (filename == "core") {
        filename <- "ar_index_global_prof.txt.gz"
    } else if (filename == "bgcargo" || filename == "bgc") {
        filename <- "argo_bio-profile_index.txt.gz"
    } else if (filename == "merge" || filename == "merged") {
        stop("in getIndex() :\n Merged datasets are no longer available. Try using filename='synthetic'", call.=FALSE)
    } else if (filename == "synthetic") {
        filename <- "argo_synthetic-profile_index.txt.gz"
    } else if (filename == "traj") {
        filename <- "ar_index_global_traj.txt.gz"
    } else if (filename == "bio-traj") {
        filename <- "argo_bio-traj_index.txt.gz"
    }

    if (!(filename %in% names))
        stop("filename=\"", filename, "\" doesn't exist. Try one of these: \"core\", \"bgc\", \"bgcargo\", \"traj\", \"bio-traj\", \"synthetic\",\"ar_index_global_prof.txt.gz\", \"argo_bio-profile_index.txt.gz\", \"ar_index_global_traj.txt.gz\", \"argo_bio-traj_index.txt.gz\", or \"argo_synthetic-profile_index.txt.gz\".")
    if (filename != filenameOrig)
        argoFloatsDebug(debug, "Converted filename='", filenameOrig, "' to filename='", filename, "'.\n", sep="")
    ## Note: 'url' may contain more than one element
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
    argoFloatsDebug(debug, "Set destfileRda=\"", destfileRda, "\".\n", sep="")
    res@metadata$url <- url[1]
    res@metadata$header <- NULL
    if (argoFloatsIsCached(filenameOrig, debug=debug)) {
        argoFloatsDebug(debug, "using an index that is cached in memory for this R session\n")
        return(argoFloatsGetFromCache(filenameOrig, debug=debug))
    }
    ## See if we have an .rda file that is sufficiently youthful.
    if (file.exists(destfileRda)) {
        destfileAge <- (as.integer(Sys.time()) - as.integer(file.info(destfileRda)$mtime)) / 86400 # in days
        argoFloatsDebug(debug, "This destfileRda already exists, and its age is ", round(destfileAge, 3), " days\n", sep="")
        if (destfileAge < age) {
            argoFloatsDebug(debug, "Using existing destfileRda, since its age is under ", age, " days\n", sep="")
            argoFloatsDebug(debug, "The local .rda file\n    '", destfileRda, "'\n", sep="")
            argoFloatsDebug(debug, "is not being updated from\n    ", url[1], "\n", showTime=FALSE)
            argoFloatsDebug(debug, "because it is only", round(destfileAge, 4), "days old.\n", showTime=FALSE)
            argoFloatsDebug(debug, "About to load '", destfileRda, "'.\n", sep="")
            argoFloatsIndex <- NULL # defined again in next line; this is to quieten code-diagnostics
            load(destfileRda)
            argoFloatsDebug(debug, "Finished loading '", destfileRda, "'.\n", sep="")
            res@metadata$server <- server[1]
            res@metadata$ftpRoot <- argoFloatsIndex[["ftpRoot"]]
            res@metadata$header <- argoFloatsIndex[["header"]]
            res@data$index <- argoFloatsIndex[["index"]]
            argoFloatsDebug(debug, "storing this index in memory for this R session\n")
            argoFloatsStoreInCache(filenameOrig, res, debug=debug)
            argoFloatsDebug(debug, "} # getIndex()\n", style="bold", showTime=FALSE, unindent=1)
            return(res)
        }
        argoFloatsDebug(debug, sprintf("Must update destfileRda, since its age exceeds %.2f days\n", age))
    }
    ## We need to download data. We do that to a temporary file, because we will be saving
    ## an .rda file, not the data on the server.
    destfileTemp <- tempfile(pattern="argo", fileext=".gz")
    argoFloatsDebug(debug, "Allocated temporary file\n    '", destfileTemp, "'.\n", sep="")
    failedDownloads <- 0
    iurlSuccess <- 0                   # set to a positive integer in the following loop, if we succeed
    for (iurl in seq_along(url)) {
        argoFloatsDebug(debug, "About to try downloading index file from\n    '", url[iurl], "'.\n", sep="")
        if (!quiet) message(sprintf("Downloading '%s'", url[iurl]))
        status <- try(curl::curl_download(url=url[iurl],
                                          destfile=destfileTemp,
                                          mode="wb"))
        ## status <- capture.output(try(curl::curl_download(url=url[iurl],
        ##                                                  destfile=destfileTemp,
        ##                                                  mode="wb"),
        ##                              silent=!TRUE),
        ##                          type="message")
        if (!inherits(status, "try-error")) {
            if (failedDownloads > 0)
                message("Downloaded index from ", url[iurl])
            iurlSuccess <- iurl
            break                      # the download worked
        } else if (any(grepl("application callback", status))) {
            stop(status)
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
    ## message(names)
    ##if (grepl("merge", filename)) {
    ##     names <- c("file", "date", "latitude", "longitude", "ocean",
    ##                "profiler_type", "institution", "parameters",
    ##                "param_data_mode", "date_update")
    ##     argoFloatsDebug(debug, "Skipping (flawed) header in the merged file.\n", sep="")
    ## }
    argoFloatsDebug(debug, "Reading index file contents (can be slow).\n", sep="")
    ##    print(system.time(
    index <- read.csv(destfileTemp, skip=2 + lastHash, col.names=names, stringsAsFactors=FALSE, colClasses="character")
    ##))
    argoFloatsDebug(debug, "Setting out-of-range longitude and latitude to NA.\n")
    if ("latitude" %in% names(index)) {
        index$latitude <- as.numeric(index$latitude)
        index$latitude[abs(index$latitude) > 90] <- NA
    } else if ("latitude_max" %in% names(index) && "latitude_min" %in% names(index)) {
        index$latitude_max <- as.numeric(index$latitude_max)
        index$latitude_min <- as.numeric(index$latitude_min)
        #message("the class of lat min is", class(index$latitude_min), "and the class of lat max is", class(index$latitude_max))
    } else {
        stop("Misconfigured index file: no \"latitude\" data found")
    }
    if ("longitude" %in% names(index)) {
        index$longitude <- as.numeric(index$longitude)
        index$longitude[abs(index$longitude) > 360] <- NA
    } else if ("longitude_max" %in% names(index) && "longitude_min" %in% names(index)) {
        index$longitude_max <- as.numeric(index$longitude_max)
        index$longitude_min <- as.numeric(index$longitude_min)
    } else {
        stop("Misconfigured index file: no \"longitude\" data found")
    }
    argoFloatsDebug(debug, "Decoding dates.\n", sep="")

    ## 2020-12-12: switch from as.POSIXct() to lubridate::fast_strptime(), shaving typically
    ## 1 second from elapsed times.  See below for some timing tests, and
    ## https://github.com/ArgoCanada/argoFloats/issues/334 for discussion.
    ##
    ##  Note that we are only 'Suggest'ing lubridate in DESCRIPTION (and requiring it here) because otherwise the user will
    ##  see possibly scary messages about function overriding.
    ##
    ##: cat("as.POSIXct() timing:\n")
    ##: print(system.time(t1 <- as.POSIXct(as.character(index$date), format="%Y%m%d%H%M%S", tz="UTC")))
    ##: cat("strptime() timing:\n")
    ##: print(system.time(t2 <- strptime(as.character(index$date), format="%Y%m%d%H%M%S", tz="UTC")))
    ##: cat("ymd_hms() timing:\n")
    ##: print(system.time(t3 <- lubridate::ymd_hms(as.character(index$date), quiet=TRUE)))
    ##: cat("lubridate::fast_strptime() timing:\n")
    ##: print(system.time(t4 <- lubridate::fast_strptime(as.character(index$date), format="%Y%m%d%H%M%S", lt=FALSE, tz="UTC")))
    ##: cat("readr::parse_datetime() timing:\n")
    ##: print(system.time(t5 <- readr::parse_datetime(as.character(index$date), format="%Y%m%d%H%M%S")))
    ##: cat("tests of matches:\n")
    ##: cat(" as.POSIXct() same as strptime(): ", all.equal(t1, t2), "\n")
    ##: cat(" as.POSIXct() same as lubridate::ymd_hms(): ", all.equal(t1, t3), "\n")
    ##: cat(" as.POSIXct() same as lubridate::fast_strptime(): ", all.equal(t1, t4), "\n")
    ##: cat(" as.POSIXct() same as readr::parse_datetime(): ", all.equal(t1, t4), "\n")
    ##
    ## as.POSIXct() timing:
    ##    user  system elapsed
    ##   2.841   0.048   2.913
    ##   2.793   0.051   2.867
    ##   2.850   0.041   2.899
    ## strptime() timing:
    ##    user  system elapsed
    ##   2.435   0.008   2.451
    ##   2.426   0.002   2.429
    ##   2.440   0.002   2.444
    ## ymd_hms() timing:
    ##    user  system elapsed
    ##   3.383   0.101   3.496
    ##   3.340   0.102   3.445
    ##   3.399   0.095   3.499
    ## lubridate::fast_strptime() timing:
    ##    user  system elapsed
    ##   1.894   0.003   1.903
    ##   1.860   0.002   1.865
    ##   1.913   0.002   1.918
    ## readr::parse_datetime() timing:
    ## Warning: 1667 parsing failures.
    ##   row col               expected        actual
    ##  8956  -- date like %Y%m%d%H%M%S 2.0040404e+13
    ##  8967  -- date like %Y%m%d%H%M%S 2.0040723e+13
    ##  9025  -- date like %Y%m%d%H%M%S 2.0040423e+13
    ## 13984  -- date like %Y%m%d%H%M%S 2.0041001e+13
    ## 14024  -- date like %Y%m%d%H%M%S 2.0040325e+13
    ## ..... ... ...................... .............
    ## See problems(...) for more details.
    ##
    ##    user  system elapsed
    ##   3.324   0.083   3.475
    ##   3.293   0.080   3.433
    ##   3.067   0.080   3.182
    if (!requireNamespace("lubridate", quietly=TRUE))
        stop("must install.packages(\"lubridate\") for getIndex() to work")
    index$date <- lubridate::fast_strptime(as.character(index$date), format="%Y%m%d%H%M%S", lt=FALSE, tz="UTC")
    index$date_update <- lubridate::fast_strptime(as.character(index$date_update), format="%Y%m%d%H%M%S", lt=FALSE, tz="UTC")

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
    res@metadata$ftpRoot <- argoFloatsIndex$ftpRoot
    res@metadata$header <- argoFloatsIndex$header
    res@data$index <- argoFloatsIndex$index
    res@processingLog <- oce::processingLogAppend(res@processingLog,
                                                  paste("getIndex(server=",
                                                        if (length(serverOrig) == 1) paste("\"", serverOrig, "\", ", sep="")
                                                        else paste("c(\"", paste(serverOrig, collapse="\", \""), "\"), ", sep=""),
                                                        "filename=\'", filename, "\", age=", age, ")", sep=""))
    argoFloatsDebug(debug, "storing newly-read index in memory for this R session\n")
    argoFloatsDebug(debug, "} # getIndex()\n", style="bold", unindent=1)
    argoFloatsStoreInCache(filenameOrig, res, debug=debug)
    res
}


#' Get "cycles"/"trajectory" Files Named in an argoFloats Index
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
#' a different pattern on the USGODAE an Ifremer servers, and
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
#' For more on this function, see Kelley et al. (2021).
#'
#' @param index an [`argoFloats-class`] object of type `"index"`, as created
#' by [getIndex()].
#'
#' @template destdir
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
#' which holds the URLs from which the NetCDF
#' files were downloaded, and `file`, which
#' holds the path names of the downloaded files; the latter
#' is used by [readProfiles()].
#'
## NOTE: this will take longer than 5s, so I had marked it dontrun on an initial
## CRAN submission.  THe CRAN review suggested marking it donttest instead, but
## when I do that, rhub::check_for_cran() gives an error on the getIndex, relating
## to the non-existence of ~/data/argo.  But, really, what's the point of these
## examples?  The docs are really quite straightforward, and we have a paper,
## besides. I don't see that these examples illustrate anything that the docs
## don't already illustrate, and so I have removed them. --DK
## @examples
##\dontrun{
## # This is not run, because it takes longer than 5s to complete.
## library(argoFloats)
## data(index)
## index2 <- subset(index, 1:2)
## profiles2 <- getProfiles(index2)
## # See ?readProfiles for how to read the files now downloaded.
##}
#'
#' @references
#' Kelley, D. E., Harbin, J., & Richards, C. (2021). argoFloats: An R package for analyzing
#' Argo data. Frontiers in Marine Science, (8), 636922.
#' \doi{10.3389/fmars.2021.635922}
#'
#' @author Dan Kelley
#'
## @importFrom oce processingLogAppend vectorShow
#'
#' @export
getProfiles <- function(index, destdir=argoDefaultDestdir(), age=argoDefaultProfileAge(), retries=3, skip=TRUE, quiet=TRUE, debug=0)
{
    if (!inherits(index, "argoFloats") || index[["type"]] != "index")
        stop("'index' must be an object created with getIndex() or subset()")
    debug <- max(0, min(3, floor(debug+0.5)))
    if (!requireNamespace("oce", quietly=TRUE))
        stop("must install.packages(\"oce\") for getProfiles() to work")
    n <- length(index@data$index)
    argoFloatsDebug(debug,  "getProfiles() {\n", style="bold", showTime=FALSE, unindent=1)
    if (missing(index))
        stop("In getProfiles() : must provide an index, as created by getIndex()", call.=FALSE)
    if (!inherits(index, "argoFloats"))
        stop("'index' must be an object created by getIndex()")
    res <- new("argoFloats", type="profiles")
    n <- length(index[["file"]])
    if (n == 0) {
        warning("In getProfiles() : the index has no files, so there is nothing to 'get'\n", call.=FALSE)
        file <- character(0)
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
        ## the NetCDF files from the same source as the index, and *not* from the source listed
        ## in the "# FTP" line in the header within that index.
        server <- index[["server"]]
        ## I *thought* the USGODAE and IFREMER servers were once set up differently, with only usgodae having "dac" in the path
        ## name.  That is why the next block was written.  However, as of May 15, 2020, it seems they are set up in the same
        ## way, so the Ifremer case was rewritten to match the usgodae case.
        urls <- paste0(server, "/dac/", index[["file"]])
        argoFloatsDebug(debug, oce::vectorShow(urls))
        file <- downloadWithRetries(urls, destdir=destdir, destfile=basename(urls),
                                    quiet=quiet, age=age, async=TRUE, debug=debug-1)
    }
    res@metadata$destdir <- destdir
    res@data$url <- urls
    res@data$file <- file
    res@processingLog <- oce::processingLogAppend(res@processingLog, "getProfiles(index, ...)")
    argoFloatsDebug(debug,  "} # getProfiles()\n", style="bold", showTime=FALSE, unindent=1)
    res
}
