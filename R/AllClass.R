# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' A Package for Processing Argo Float Profiles
#'
#' The `argoFloats` package provides tools for downloading and processing Argo profile data.
#' It allows users to focus on core, biogeochemical ("BGC"), or deep Argo profiles, and
#' also to sift these profiles based on ID, time, geography, variable, institution, and ocean, etc.
#' Once downloaded, such datasets can be analyzed within `argoFloats` or using other R tools
#' and packages.
#'
#' The development website is \url{https://github.com/ArgoCanada/argoFloats}, and
#' \url{https://argocanada.github.io/argoFloats/index.html} provides a simpler view
#' that may be more helpful to most users. For more on the argoFloats package, see
#' Kelley et al. (2021).
#'
#' The sketch given below illustrates the typical workflow with the package, with
#' descriptions of the steps on the left, and names of the relevant functions
#' on the right.
#'
#' \if{html}{\figure{workflow.png}{options: width="455" alt="Figure: workflow.png"}}
#'
#' As illustrated, the central functions are named
#' [getIndex()], [subset()][subset,argoFloats-method()],
#' [getProfiles()], and [readProfiles()], and so a good way to
#' get familiar with the package is to read their documentation entries
#' and try the examples provided therein.  Some built-in datasets are provided
#' for concreteness of illustration and for testing, but actual work always
#' starts with a call to [getIndex()] to download a full index of float data.
#'
#' In addition to these functions, `argoFloats` also provides
#' specialized versions of R "generic" functions, as follows.
#'
#' 1. `[[` provides a way to extract items from `argoFloats` objects,
#' without getting lost in the details of storage.
#' See \code{\link{[[,argoFloats-method}} for details.
#' (Note that `[[<-` is *not* specialized, since the
#' user is highly discouraged from altering values within `argoFloats`
#' objects.)
#'
#' 2. `plot()` provides simple ways to plot aspects of [`argoFloats-class`] objects.
#' See [plot,argoFloats-method()] for details.
#'
#' 3. `summary()` displays key features of [`argoFloats-class`] objects.
#' See [summary,argoFloats-method()] for details.
#'
#' 4. `show()` provides a one-line sketch of [`argoFloats-class`] objects.
#' See [show,argoFloats-method()] for details.
#'
#' 5. `merge()` combines multiple index objects into a new index object.
#'
#' It should be noted that the profile elements within `argoFloats` objects are stored as
#' in the form of `argo` objects as defined by the `oce` package.
#' This means that `argoFloats` users can rely on
#' a wide variety of `oce` functions to analyze their data.
#' The full suite of R tools is also available, and the vastness of
#' that suite explains why `argoFloats` is written in R.
#'
#' Kelley, D. E., Harbin, J., & Richards, C. (2021). argoFloats: An R package for analyzing
#' Argo data. Frontiers in Marine Science, (8), 636922.
#' \doi{10.3389/fmars.2021.635922}
#'
#' @importFrom methods new
#' @name argoFloats
#' @docType package
#' @keywords internal
"_PACKAGE"
## usethis namespace: start
## usethis namespace: end
NULL


#' A Sample Index of Profiles
#'
#' This was created by subsetting a global index to the Argo profiles
#' that were within a 200km radius of Marsh Harbour, Abaco Island,
#' Bahamas, using the following code.
#' ```
#  library(argoFloats)
#' indexAll <- getIndex()
#' index <- subset(indexAll,
#'     circle=list(longitude=-77.06, latitude=26.54, radius=200))
#' ```
#'
#' @template server_caveat
#'
#' @examples
#' library(argoFloats)
#' data(index)
#' plot(index, bathymetry = FALSE)
#' @name index
#' @docType data
#' @family datasets provided with argoFloats
NULL


#' A Sample Index of Biogeochemical-Argo Profiles
#'
#' This was created by subsetting a global index to the BGC Argo profiles
#' that were within a 300km radius of Marsh Harbour, Abaco Island,
#' Bahamas, using the following code.
#' ```
#' library(argoFloats)
#' indexAll <- getIndex("bgc")
#' indexBgc <- subset(indexAll,
#'     circle=list(longitude=-77.06, latitude=26.54, radius=300))
#' ```
#'
#' @template server_caveat
#'
#' @examples
#' library(argoFloats)
#' data(indexBgc)
#' plot(indexBgc, bathymetry = FALSE)
#' summary(indexBgc)
#' unique(indexBgc[["parameters"]])
#' @name indexBgc
#' @docType data
#' @family datasets provided with argoFloats
NULL


## A sample index of merged Argo and biogeochemical-Argo profiles
##
## This was created by subsetting a global index to the BGC Argo profiles
## that were within a 300km radius of Marsh Harbour, Abaco Island,
## Bahamas, using the following code.
## ```
## library(argoFloats)
## indexAll <- getIndex("merged")
## indexMerged <- subset(indexAll,
##     circle=list(longitude=-77.06, latitude=26.54, radius=300))
## ```
##
## @template server_caveat
##
## @examples
## library(argoFloats)
## data(indexMerged)
## plot(indexMerged, bathymetry=FALSE)
## summary(indexMerged)
## unique(indexMerged[["parameters"]])
## @name indexMerged
## @docType data
## @family datasets provided with argoFloats
NULL

#' A Sample Index of Synthetic Profiles
#'
#' This was created by subsetting a global index to the BGC Argo profiles
#' that were within a 300km radius of Marsh Harbour, Abaco Island,
#' Bahamas, using the following code.
#' ```
#' library(argoFloats)
#' indexAll <- getIndex("synthetic")
#' indexSynthetic <- subset(indexAll,
#'     circle=list(longitude=-77.06, latitude=26.54, radius=300))
#' ```
#' This "synthetic" type of index is a replacement for the older "merged" index.  See
#' `http://www.argodatamgt.org/Data-Mgt-Team/News/BGC-Argo-synthetic-profiles-distributed-on-GDAC`
#' for more on the data file format, the reasons for the change, and the
#' timetable for the transition from "merged".
#'
#' @template server_caveat
#'
#' @examples
#' library(argoFloats)
#' data(indexSynthetic)
#' plot(indexSynthetic, bathymetry = FALSE)
#' summary(indexSynthetic)
#' unique(indexSynthetic[["parameters"]])
#' @name indexSynthetic
#' @docType data
#' @family datasets provided with argoFloats
NULL

#' A Sample Index of Deep Argo
#'
#' This was created by subsetting a global index to the deep Argo profiles
#' that were within a 800km radius of Antarctica (67S,105E),
#' using the following code.
#' ```
#' library(argoFloats)
#' subset <- subset(getIndex(), deep=TRUE)
#' sub2 <- subset(subset, circle=list(longitude=105, latitude=-67, radius=800))
#' ```
#'
#' @template server_caveat
#'
#' @examples
#' library(argoFloats)
#' data(indexDeep)
#' plot(indexDeep, bathymetry = FALSE)
#' summary(indexDeep)
#' @name indexDeep
#' @docType data
#' @family datasets provided with argoFloats
NULL

#' Base Class for argoFloats Objects
#'
#' In the normal situation, users will not create these objects directly. Instead,
#' they are created by functions such as [getIndex()].
#'
#' @slot data The `data` slot is a list with contents that vary with the object type.  For example,
#' [getIndex()] creates objects of type `"index"` that have a single unnamed
#' element in `data` that is a data frame. This data frame has a column named `file`
#' that is used in combination with `metadata@ftpRoot` to form a URL for downloading,
#' along with columns named `date`, `latitude`, `longitude`,
#' `ocean`, `profiler_type`, `institution` and `date_update`. Other "get" functions
#' create objects with different contents.
#'
#' @slot metadata The `metadata` slot is a list containing information about the data. The contents vary
#' between objects and object types.  That type is indicated by elements named
#' `type` and `subtype`, which are checked by many functions within the package.
#'
#' @slot processingLog The `processingLog` slot is a list containing time-stamped processing steps that may be
#' stored in the object by argoFloats functions. These are noted in `summary()` calls.
#'
#' @examples
#' str(new("argoFloats"))
setClass("argoFloats", slots = c(metadata = "list", data = "list", processingLog = "list"))

setMethod(
    f = "initialize",
    signature = "argoFloats",
    definition = function(.Object, type = "unspecified", subtype = "cycles") {
        .Object@data <- list()
        .Object@metadata$type <- type
        .Object@metadata$subtype <- subtype
        .Object@processingLog$time <- as.POSIXct(Sys.time())
        .Object@processingLog$value <- "create 'argoFloats' object"
        return(.Object)
    }
)


#' Look up a Value Within an argoFloats Object
#'
#' This function provides an easy way to look up values within an [`argoFloats-class`]
#' object, without the need to know the exact structure of the data. The action
#' taken by `[[` depends on the `type` element in the `metadata` slot of
#' the object (which is set by the function that created the object), on the
#' value of `i` and, in some cases, on the value of `j`; see \dQuote{Details}.
#'
#' There are several possibilities, depending on the object `type`, as
#' listed below. Note that these possibilities are checked in the
#' order listed here.
#'
#' 1. For all object types:
#'     1. If `i` is `"metadata"` then the `metadata` slot of `x` is returned.
#'     2. Otherwise, if `i` is `"data"` then the `data` slot of `x` is returned.
#'     3. Otherwise, if `i` is `"cycle"` then a character vector of the cycle
#'        numbers is returned.
#'     4. Otherwise, if `i` is `"processingLog"` then the `processingLog` slot of
#'        `x` is returned.
#'     5. Otherwise, if `i` is `"ID"` then a character vector of the ID numbers
#'        is returned.
#'     6. Otherwise, the following steps are taken, depending on `type`.
#'
#' 2. If `type` is `"index"`, i.e. if `x` was created with [getIndex()]
#' or with [subset,argoFloats-method()] acting on the result of [getIndex()],
#' then:
#'     1. If `i` is numeric and `j` is unspecified, then `i` is taken to
#'        be an index that identifies the row(s) of the data frame that
#'        was constructed by [getIndex()] based on the remote index file
#'        downloaded from the Argo server.  This has elements `file` for
#'        the remote file name, `date` for the date of the entry, `latitude`
#'        and `longitude` for the float position, etc.
#'     3. If `i` is the name of an item in the `metadata` slot, then that item
#'        is returned. The choices are:
#'        `"destdir"`, `"destfileRda"`, `"filename"`, `"ftpRoot"`, `"header"`,
#'        `"server"`, `"type"`, and `"url"`.
#'     4. Otherwise, if `i` is the name of an item in the `data` slot, then that item
#'        is returned.  The choices are:
#'        `"date"`, `"date_update"`, `"file"`, `"institution"`, `"latitude"`,
#'        `"longitude"`, `"ocean"`, and `"profiler_type"`. Note that `"time"` and
#'        `"time_update"` may be used as synonyms for `"date"` and `"date_update"`.
#'     5. Otherwise, if `i=="index"` then that item from the `data` slot of `x` is returned.
#'        (For the possible names, see the previous item in this sub-list.)
#'     6. Otherwise, if `i` is an integer, then the `i`-th row of the `index` item in
#'        the `data` slot is returned.  This is a good way to learn the
#'        longitude and latitude of the profile, the file name on the server,
#'        etc.
#'     7. Otherwise, if `i` is `"ID"` then the return value is developed from the
#'        `index$file` item within the `data` slot of `x`, in one of three cases:
#'         1. If `j` is not supplied, the return value is a vector holding the
#'            identifiers (character codes for numbers) for all the data files
#'            referred to in the index.
#'         2. Otherwise, if `j` is numeric, then the return value is a subset of
#'            the ID codes, as indexed by `j`.
#'         3. Otherwise, an error is reported.
#'     8. If `i` is `"length"`, the number of remote files pointed to by the index
#'        is returned.
#' 3. Otherwise, if `type` is `"profiles"`, i.e. if `x` was created with [getProfiles()], then:
#'     1. If `i` is numeric and `j` is unspecified, then return the local file name(s)
#'        that are identified by using `i` as an index.
#'     2. If `i` is the name of an item in the `metadata` slot, then that item
#'        is returned. The choices are:
#'        `"type"` and `"destdir"`.
#'     3. Otherwise, if `i` is the name of an item in the `data` slot, then that item
#'        is returned.  There is only one choice: `"file"`.
#'     4. If `i` is `"length"`, the number of local file names that were downloaded
#'        by [getProfiles()] is returned.
#' 4. Otherwise, if `type` is `"argos"`, i.e. if `x` was created with [readProfiles()], then:
#'     1. If `i` is equal to `"argos"`, and `j` is unspecified, then a list
#'        holding the [oce::argo-class] objects stored within `x` is returned.
#'     2. If `i` is equal to `"argos"`, and `j` is provided, then the associated
#'        [oce::argo-class] object is returned.
#'     3. If `i` is numeric and `j` is unspecified, then return the argo objects identified
#'        by using `i` as an index.
#'     4. If `i` is the name of an item in the `metadata` slot, then that item
#'        is returned. There is only choice, `"type"`.
#'     5. Otherwise, if `i` is the name of an item in the `data` slot, then that item
#'        is returned as a named list.  (At present, there is only one choice: `"argos"`.)
#'     6. Otherwise, if `i` is `"length"` then the number of oce-type argo objects in `x` is returned.
#'     7. Otherwise, if `i` is a character value then it is taken to be
#'        an item within the `metadata` or `data` slots of the argo objects
#'        stored in `x`, and the returned value is a list containing that
#'        information with one (unnamed) item per profile.
#'        If `j` is provided
#'        and equal to `"byLevel"`, then the items from the `metadata` are
#'        repeated (if necessary) and formed into matrix or vector of the same
#'        shape as the `"pressure"` field; this can be convenient for computations
#'        involving items that only occur once per profile, such as `"longitude"`,
#'        but it should not be used for items that are not level-specific, such
#'        as the various `"HISTORY_*"` elements, which apply to a dataset, not to
#'        a level
#'     8. Otherwise, NULL is reported.
#' 5. Otherwise, an error is reported.
# . For more on this function, see Kelley et al. (2021).
#'
#' @param x an [`argoFloats-class`] object.
#' @param i a character value that specifies the item to be looked up;
#' see \dQuote{Details}.
#' @param j supplemental index value, used for some `x` types
#' and `i` values; see \dQuote{Details}.
#' @param ... ignored.
#'
#' @examples
#' data(index)
#' # Full remote filename for first two item in index
#' paste0(index[["server"]], "/dac/", index[["cycle", 1:2]])
#' # File names and geographical locations of first 5 items in index
#' index5 <- subset(index, 1:5)
#' data.frame(
#'     file = gsub(".*/", "", index5[["file"]][1]),
#'     lon = index5[["longitude"]],
#'     lat = index5[["latitude"]]
#' )
#'
#' @references
#' Kelley, D. E., Harbin, J., & Richards, C. (2021). argoFloats: An R package for analyzing
#' Argo data. Frontiers in Marine Science, (8), 636922.
#' \doi{10.3389/fmars.2021.635922}
#'
#' @return the indicated item, or NULL if it is neither stored within the first argument
#' nor computable from its contents.
#'
#' @export
#'
#' @docType methods
#'
#' @aliases [[,argoFloats-method
#'
#' @author Dan Kelley
setMethod(
    f = "[[",
    signature(x = "argoFloats", i = "ANY", j = "ANY"),
    definition = function(x, i, j, ...) {
        if (missing(i)) {
            stop("Must name an item to retrieve, e.g. 'x[[\"latitude\"]]'", call. = FALSE)
        }
        dots <- list(...)
        debug <- if (!is.null(dots$debug)) dots$debug else 0
        argoFloatsDebug(debug, "[[,argoFloats-method with i=", i, " and j=", if (missing(j)) "(missing)" else j, "\n", sep = "", style = "bold", unindent = 1)
        if (length(i) == 1 && i == "metadata") {
            argoFloatsDebug(debug, "} # returned metadata slot\n", style = "bold", unindent = 1)
            return(x@metadata)
        }
        if (length(i) == 1 && i == "data") {
            argoFloatsDebug(debug, "} # returned data slot\n", style = "bold", unindent = 1)
            return(x@data)
        }
        if (length(i) == 1 && i == "processingLog") {
            argoFloatsDebug(debug, "} # returned processingLog slot\n", style = "bold", unindent = 1)
            return(x@processingLog)
        }
        type <- x@metadata$type
        istraj <- identical(x@metadata$subtype, "trajectories")
        if (type == "index") {
            argoFloatsDebug(debug, "Handling type \"index\" case (istraj=", istraj, ").\n", sep = "")
            if (is.numeric(i)) {
                return(x@data$index[i, ])
            } else if (length(i) == 1 && i == "index") {
                return(x@data$index)
            } else if (length(i) == 1 && i %in% names(x@metadata)) {
                return(x@metadata[[i]])
            } else if (length(i) == 1 && i %in% names(x@data$index)) {
                return(if (missing(j)) x@data$index[[i]] else x@data$index[[i]][j])
            } else if (length(i) == 1 && i == "profile") {
                stop("In [[ : the old syntax [[\"profile\"]] is no longer converted to [[\"cycle\"]]")
                ## changed to stop() 2020-10-07
                ## warning("converted x[[\"profile\"]] to x[[\"cycle\"]] for backwards compatibility;\n  NOTE: this conversion will cease after September, 2020.")
                ## cycle <- gsub("^.*[/\\\\][A-Z]*[0-9]*_([0-9]{3,4}[D]{0,1})\\.nc$", "\\1", x@data$index$file)
                ## return(as.vector(if (missing(j)) cycle else cycle[j]))
            } else if (length(i) == 1 && i == "cycle" && !istraj) {
                cycle <- gsub("^.*[/\\\\][A-Z]*[0-9]*_([0-9]{3,4}[D]{0,1})\\.nc$", "\\1", x@data$index$file, perl = TRUE)
                return(as.vector(if (missing(j)) cycle else cycle[j]))
            } else if (length(i) == 1 && i == "ID" && !istraj) {
                ID <- gsub("^.*[/\\\\][A-Z]*([0-9]*)_[0-9]{3,4}[A-Z]*\\.nc$", "\\1", x@data$index$file, perl = TRUE)
                # test told <- system.time({IDold <- gsub("^.*[/\\\\][A-Z]*([0-9]*)_[0-9]{3,4}[A-Z]*\\.nc$", "\\1", x@data$index$file)})
                # test tnew <- system.time({ID <- gsub("^.*[/\\\\][A-Z]*([0-9]*)_[0-9]{3,4}[A-Z]*\\.nc$", "\\1", x@data$index$file, perl=TRUE)})
                # test message("old=", told[1], "s, new=", tnew[1], "s (", round(told[1]/tnew[1],2), "X faster)")
                # test stopifnot(all.equal(IDold, ID))
                return(as.vector(if (missing(j)) ID else ID[j]))
            } else if (length(i) == 1 && i == "ID" && istraj) {
                ID <- gsub("^.*[/\\\\][A-Z]*([0-9]*)_[A-Z]*traj*\\.nc$", "\\1", x@data$index$file)
                return(as.vector(if (missing(j)) ID else ID[j]))
            } else if (length(i) == 1 && i == "length") {
                return(length(x@data$index$file))
            } else if (length(i) == 1 && i == "parameters") {
                stop("there are no parameters for core Argo index objects. Try BGC, Merged, or Synthetic Argo.", call. = FALSE)
            } else if (length(i) == 1 && i == "time") {
                return(x@data$index$date)
            } else if (length(i) == 1 && i == "time_update") {
                return(x@data$index$date_update)
            } else {
                subtype <- if (is.null(x@metadata$subtype)) "cycles" else x@metadata$subtype
                stop("no \"", paste(i, collapse = ","), "\" in an object of type=\"index\" and subtype=\"", subtype, "\"", call. = FALSE)
            }
        } else if (type == "profiles") { # made by getProfiles()
            argoFloatsDebug(debug, "Handling type==\"profiles\" case.\n")
            if (is.numeric(i) && missing(j)) {
                return(x@data$file[[i]])
            } else if (length(i) == 1 && i == "cycle") {
                cycle <- gsub("^.*[/\\\\][A-Z]*[0-9]*_([0-9]{3,4})[A-Z]*\\.nc$", "\\1", x[["file"]])
                return(as.vector(if (missing(j)) cycle else cycle[j]))
            } else if (length(i) == 1 && i == "ID") {
                ID <- gsub("^.*[/\\\\][A-Z]*([0-9]*)_[0-9]{3,4}[A-Z]*\\.nc$", "\\1", x[["file"]])
                return(as.vector(if (missing(j)) ID else ID[j]))
            } else if (length(i) == 1 && i %in% names(x@metadata)) {
                return(x@metadata[[i]])
            } else if (length(i) == 1 && i %in% names(x@data)) {
                if (missing(j)) {
                    return(x@data[[i]])
                } else if (is.numeric(j)) {
                    return(x@data[[i]][j])
                } else {
                    stop("if j is given, it must be numeric")
                }
            } else if (length(i) == 1 && i == "length") {
                return(length(x@data$file))
            } else {
                stop("cannot interpret i=", paste(i, collapse = ","), " for an object of type=\"", type, "\"")
            }
        } else if (type == "argos") { # made by readProfiles()
            argoFloatsDebug(debug, "Handling type==\"argos\" case.\n")
            if (is.numeric(i) && missing(j)) {
                return(x@data$argos[[i]])
            } else if (length(i) == 1 && i %in% names(x@metadata)) {
                argoFloatsDebug(debug, "i=\"", i, "\" is in metadata\n")
                return(x@metadata[[i]])
            } else if (length(i) == 1 && i %in% names(x@data)) {
                argoFloatsDebug(debug, "i=\"", i, "\" is in data\n")
                return(if (missing(j)) x@data[[i]] else x@data[[i]][[j]])
            } else if (length(i) == 1 && i == "cycle") {
                argoFloatsDebug(debug, "i=\"", i, "\"\n", sep = "")
                cycle <- gsub("^.*[/\\\\][A-Z]*[0-9]*_([0-9]{3,4})[A-Z]*\\.nc$", "\\1", unlist(x[["filename"]]))
                return(as.vector(if (missing(j)) cycle else cycle[j]))
            } else if (length(i) == 1 && i == "length") {
                return(length(x@data$argos))
            } else if (length(i) == 1 && i == "filename") {
                argoFloatsDebug(debug, "i=\"", i, "\" is detected specifically\n")
                ID <- unlist(lapply(x@data$argos, function(a) a[["filename"]]))
                return(as.vector(if (missing(j)) ID else ID[j]))
            } else if (length(i) == 1 && i == "ID") {
                ID <- unlist(lapply(x@data$argos, function(a) a[["id"]][1])) # IMPORTANT: oce calls it "id"
                return(as.vector(if (missing(j)) ID else ID[j]))
            } else {
                argoFloatsDebug(debug, "No special argoFloats::[[ case found, so will try oce::[[ on each item within x@data$argos.\n")
                if (!missing(j)) {
                    argoFloatsDebug(debug, "WARNING: j was given, so the results may be wrong.\n")
                    if (j != "byLevel") {
                        stop("[[\"", i, "\"]], j]] requires that j be \"byLevel\", not \"", j, "\"")
                    }
                    return(lapply(
                        x[["argos"]],
                        function(a) {
                            pressure <- a[["pressure"]]
                            if (is.matrix(pressure)) {
                                matrix(a[[i]], nrow = nrow(pressure), ncol = ncol(pressure))
                            } else {
                                rep(a[[i]], length.out = length(pressure))
                            }
                        }
                    ))
                } else {
                    if (i == "longitude" || i == "latitude") {
                        argoFloatsDebug(debug, "returning", i, "from the oce::argo object metadata\n")
                        RES <- lapply(x@data$argos, function(a) a[[i]])
                        argoFloatsDebug(debug, "}\n", style = "bold", unindent = 1)
                        return(RES)
                    } else {
                        argoFloatsDebug(debug, "Determining \"", i, "\" with oce::as.ctd() and then oce::[[,ctd-method.\n", sep = "")
                        if (!requireNamespace("oce", quietly = TRUE)) {
                            stop("must install.packages(\"oce\"), for [[ to work")
                        }
                        ## OLD computable <- c("CT", "SA", "sigmaTheta", "spice")
                        RES <- lapply(
                            x[["argos"]],
                            function(a) {
                                a[[i]] # note that this might be i, or adjusted i
                            }
                        )
                        argoFloatsDebug(debug, "} # [[,argoFloats-method\n", style = "bold", unindent = 1)
                        return(RES)
                    }
                }
            }
        } else {
            stop("the object type must be \"index\", \"profiles\", or \"argos\", but it is \"", type, "\"")
        }
        if (i %in% c("longitude", "latitude")) {
            res <- if (type == "argos") {
                ## argoFloatsDebug(debug, "type=\"argos\"\n", sep="")
                unlist(lapply(x[["argos"]], function(a) rep(a[[i]], length = length(a[["salinity"]]))))
            } else if (type == "index") {
                x@data$index[[i]]
            } else {
                stop("[[\"", i, "\"]] only works for objects created by readProfiles()")
            }
            ## argoFloatsDebug(debug, "[[ for '", i, "' returning ", length(res), " values\n", sep="")
            return(res)
        }
        return(NULL)
    }
)
