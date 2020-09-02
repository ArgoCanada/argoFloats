# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' A Package for Processing Argo Float Profiles
#'
#' The `argoFloats` package provides tools for downloading and processing Argo profile data.
#' It allows users to focus on core argo, biogeochemical (bgc) argo, or deep argo profiles, and
#' also to sift these profiles based on id, time, geography, variable, institution, and ocean.
#' Once downloaded, such datasets can be analysed within `argoFloats` or using other R tools
#' and packages.
#'
#' The development website is \url{https://github.com/ArgoCanada/argoFloats}, and
#' \url{https://argocanada.github.io/argoFloats/index.html} provides a simpler view
#' that may be more helpful to most users.
#'
#' The sketch given below illustrates the typical workflow with the package, with
#' descriptions of the steps on the left, and names of the relevant functions
#' on the right.
#'
#' \if{html}{\figure{workflow.png}{options: width=455px alt="Figure: workflow.png"}}
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
#' a wide variety of `oce` functions to analyse their data.
#' The full suite of R tools is also available, and the vastness of
#' that suite explains why `argoFloats` is written in R.
#'
#' @importFrom methods new
#' @name argoFloats-package
#' @docType package
NULL


#' A sample index of profiles
#'
#' This was created by subsetting a global index to the 1788 Argo profiles
#' that were within a 200km radius of Marsh Harbour, Abaco Island,
#' Bahamas, as of 2020 March 14, using the following code.
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
#' plot(index, bathymetry=FALSE)
#' @name index
#' @docType data
#' @family datasets provided with argoFloats
NULL


#' A sample index of biogeochemical-Argo profiles
#'
#' This was created by subsetting a global index to the 39 BGC Argo profiles
#' that were within a 300km radius of Marsh Harbour, Abaco Island,
#' Bahamas, as of 2020 March 23, using the following code.
#'```
#' library(argoFloats)
#' indexAll <- getIndex("bgc")
#' indexBgc <- subset(indexAll,
#'     circle=list(longitude=-77.06, latitude=26.54, radius=300))
#'```
#'
#' @template server_caveat
#'
#' @examples
#' library(argoFloats)
#' data(indexBgc)
#' plot(indexBgc, bathymetry=FALSE)
#' summary(indexBgc)
#' unique(indexBgc[["parameters"]])
#' @name indexBgc
#' @docType data
#' @family datasets provided with argoFloats
NULL


#' A sample index of merged Argo and biogeochemical-Argo profiles
#'
#' This was created by subsetting a global index to the 39 BGC Argo profiles
#' that were within a 300km radius of Marsh Harbour, Abaco Island,
#' Bahamas, as of 2020 March 23, using the following code.
#'```
#' library(argoFloats)
#' indexAll <- getIndex("merged")
#' indexMerged <- subset(indexAll,
#'     circle=list(longitude=-77.06, latitude=26.54, radius=300))
#'```
#'
#' @template server_caveat
#'
#' @section Historical note:
#' This "merged" file from the usgodae server is likely to be removed, when
#' that server changes to the "synthetic" file format that the ifremer server
#' uses (as of May, 2020 and perhaps months previously, since the changeover
#' data was supposed to be Dec, 2019).
#' @examples
#' library(argoFloats)
#' data(indexMerged)
#' plot(indexMerged, bathymetry=FALSE)
#' summary(indexMerged)
#' unique(indexMerged[["parameters"]])
#' @name indexMerged
#' @docType data
#' @family datasets provided with argoFloats
NULL

#' A sample index of synthetic (i.e. combined) Argo and biogeochemical-argo profiles
#'
#' This was created by subsetting a global index to the 39 BGC Argo profiles
#' that were within a 300km radius of Marsh Harbour, Abaco Island,
#' Bahamas, as of 2020 May 15, using the following code.
#'```
#' library(argoFloats)
#' indexAll <- getIndex("synthetic")
#' indexSynthetic <- subset(indexAll,
#'     circle=list(longitude=-77.06, latitude=26.54, radius=300))
#'```
#' This "synthetic" type of index is a replacement for the older "merged" index.  See
#' \url{http://www.argodatamgt.org/Data-Mgt-Team/News/BGC-Argo-synthetic-profiles-distributed-on-GDAC}
#' for more on the data file format, reasons for the change, and timetable for transition
#' from "merged".
#'
#' @template server_caveat
#'
#' @examples
#' library(argoFloats)
#' data(indexSynthetic)
#' plot(indexSynthetic, bathymetry=FALSE)
#' summary(indexSynthetic)
#' unique(indexSynthetic[["parameters"]])
#' @name indexSynthetic
#' @docType data
#' @family datasets provided with argoFloats
NULL

#' A sample index of deep Argo
#'
#' This was created by subsetting a global index to the 163 deep Argo profiles
#' that were within a 800km radius of Antarctica (67S,105E), as of 2020 July 22,
#' using the following code.
#'```
#' library(argoFloats)
#' subset <- subset(getIndex(), deep=TRUE)
#' sub2 <- subset(subset, circle=list(longitude=105, latitude=-67, radius=800))
#'```
#'
#' @template server_caveat
#'
#' @examples
#' library(argoFloats)
#' data(indexDeep)
#' plot(indexDeep, bathymetry=FALSE)
#' summary(indexDeep)
#' @name indexDeep
#' @docType data
#' @family datasets provided with argoFloats
NULL

#'
#' Class to hold argoFloats objects
setClass("argoFloats", slots=c(metadata="list", data="list", processingLog="list"))


setMethod(f="initialize",
          signature="argoFloats",
          definition=function(.Object, type="unspecified") {
              .Object@metadata$type <- type
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'argoFloats' object"
              return(.Object)
          })


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
#'     4. Otherwise, if `i` is `"id"` then a character vector of the id numbers
#'        is returned.
#'     5. Otherwise, the following steps are taken, depending on `type`.
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
#'        `"longitude"`, `"ocean"`, and `"profiler_type"`.
#'     5. Otherwise, if `i=="index"` then that item from the `data` slot of `x` is returned.
#'        (For the possible names, see the previous item in this sub-list.)
#'     6. Otherwise, if `i` is an integer, then the `i`-th row of the `index` item in
#'        the `data` slot is returned.  This is a good way to learn the
#'        longitude and latitude of the profile, the file name on the server,
#'        etc.
#'     7. Otherwise, if `i` is `"id"` then the return value is developed from the
#'        `index$file` item within the `data` slot of `x`, in one of three cases:
#'         1. If `j` is not supplied, the return value is a vector holding the
#'            identifiers (character codes for numbers) for all the data files
#'            referred to in the index.
#'         2. Otherwise, if `j` is numeric, then the return value is a subset of
#'            the id codes, as indexed by `j`.
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
#'        information with one (unnamed) item per profile.  If `j` is provided
#'        and equal to `"byLevel"`, then the items from the `metadata` are
#'        repeated (if necessary) and formed into matrix or vector of the same
#'        shape as the `"pressure"` field; this can be convenient for computations
#'        involving items that only occur once per profile, such as `"longitude"`,
#'        but it should not be used for items that are not level-specific, such
#'        as the various `"HISTORY_*"` elements, which apply to a dataset, not to
#'        a level.
#'     8. Otherwise, NULL is reported.
#' 5. Otherwise, an error is reported.
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
#' data.frame(file=gsub(".*/", "", index5[["file"]][1]),
#'            lon=index5[["longitude"]],
#'            lat=index5[["latitude"]])
#'
#' @author Dan Kelley
#'
#' @export
#' @docType methods
#' @aliases [[,argoFloats-method
setMethod(f="[[",
          signature(x="argoFloats", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (missing(i))
                  stop("Must name an item to retrieve, e.g. 'x[[\"latitude\"]]'", call.=FALSE)
              dots <- list(...)
              debug <- if (!is.null(dots$debug)) dots$debug else 0
              if (length(i) == 1 && i == "metadata")
                  return(x@metadata)
              if (length(i) == 1 && i == "data")
                  return(x@data)
              type <- x@metadata$type
              if (type == "index") {
                  if (is.numeric(i)) {
                      return(x@data$index[i,])
                  } else if (length(i) == 1 && i == "index") {
                      return(x@data$index)
                  ##??} else if (length(i) == 1 && is.numeric(i)) {
                  ##??    return(x@data$index[i,])
                  } else if (length(i) == 1 && i %in% names(x@metadata)) {
                      return(x@metadata[[i]])
                  } else if (length(i) == 1 && i %in% names(x@data$index)) {
                      return(if (missing(j)) x@data$index[[i]] else x@data$index[[i]][j])
                  } else if (length(i) == 1 && i == "profile") {
                      warning("converted x[[\"profile\"]] to x[[\"cycle\"]] for backwards compatibility;\n  NOTE: this conversion will cease after 2020-Sep-01.")
                      cycle <- gsub("^[a-z]*/[0-9]*/profiles/[A-Z]*[0-9]*_([0-9]{3,4}[D]{0,1}).nc$", "\\1", x@data$index$file)
                      return(as.vector(if (missing(j)) cycle else cycle[j]))
                  } else if (length(i) == 1 && i == "cycle") {
                      cycle <- gsub("^[a-z]*/[0-9]*/profiles/[A-Z]*[0-9]*_([0-9]{3,4}[D]{0,1}).nc$", "\\1", x@data$index$file)
                      return(as.vector(if (missing(j)) cycle else cycle[j]))
                  } else if (length(i) == 1 && i == "id") {
                      id <- gsub("^[a-z]*/([0-9]*)/profiles/[A-Z]*[0-9]*_[0-9]{3}[A-Z]*.nc$", "\\1", x@data$index$file)
                      return(as.vector(if (missing(j)) id else id[j]))
                  } else if (length(i) == 1 && i == "length") {
                      return(length(x@data$index$file))
                  } else if (length(i) == 1 && i == "parameters") {
                      stop("there are no parameters for core Argo index objects. Try BGC, Merged, or Synthetic Argo.")
                  } else {
                      stop("cannot interpret i=", paste(i, collapse=","), " for an object of type=\"index\"")
                  }
              } else if (type == "profiles") { # made by getProfiles()
                  if (is.numeric(i) && missing(j)) {
                      return(x@data$file[[i]])
                  } else if (length(i) == 1 && i == "cycle") {
                      cycle <- gsub("^.*/[A-Z]*[0-9]*_([0-9]{3})[A-Z]*.nc$", "\\1", x[['file']])
                      return(as.vector(if (missing(j)) cycle else cycle[j]))
                  } else if (length(i) == 1 && i == "id") {
                      id <- gsub("^.*/[A-Z]*([0-9]*)_[0-9]{3}[A-Z]*.nc$", "\\1", x[['file']])
                      return(as.vector(if (missing(j)) id else id[j]))
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
                      stop("cannot interpret i=", paste(i, collapse=","), " for an object of type=\"", type, "\"")
                  }
              } else if (type == "argos") { # made by readProfiles()
                  if (is.numeric(i) && missing(j)) {
                      return(x@data$argos[[i]])
                  } else if (length(i) == 1 && i %in% names(x@metadata)) {
                      return(x@metadata[[i]])
                  } else if (length(i) == 1 && i %in% names(x@data)) {
                      return(if (missing(j)) x@data[[i]] else x@data[[i]][[j]])
                  } else if (length(i) == 1 && i == "cycle") {
                      cycle <- gsub("^.*/[A-Z]*[0-9]*_([0-9]{3})[A-Z]*.nc$", "\\1", unlist(x[['filename']]))
                      return(as.vector(if (missing(j)) cycle else cycle[j]))

                      ## } else if (length(i) == 1 && i == "cycle") {
                      ##  if (missing(j)) {
                      ##      return(x@data$argos)
                      ##  } else if (is.numeric(j)) {
                      ##      return(x@data$argos[[j]])
                      ##  } else {
                      ##      stop("cannot interpret i=", paste(i, collapse=","), " and j=", paste(j, collapse=", "), " for an object of type=\"", type, "\"")
                      ##  }
                  } else if (length(i) == 1 && i == "length") {
                      return(length(x@data$argos))
                  } else if (length(i) == 1 && i == "id") {
                      id <- unlist(lapply(x@data$argos, function(a) a[['id']]))
                      return(as.vector(if (missing(j)) id else id[j]))
                  } else {
                      if (!missing(j)) {
                          if (j != "byLevel")
                              stop("[[\"", i, "\"]], j]] requires that j be \"byLevel\", not \"", j, "\"")
                          return(lapply(x[["argos"]],
                                        function(a) {
                                            pressure <- a[["pressure"]]
                                            if (is.matrix(pressure))
                                                matrix(a[[i]], nrow=nrow(pressure), ncol=ncol(pressure))
                                            else
                                                rep(a[[i]], length.out=length(pressure))
                                        }))
                      } else {
                          return(lapply(x[["argos"]],
                                        function(a) {
                                            a[[i]]
                                        }))
                      }
                  }
              } else {
                  stop("the object type must be \"index\", \"profiles\", or \"argos\", but it is \"", type, "\"")
              }
              if (i %in% c("longitude", "latitude")) {
                  res <- if (type == "argos") {
                      argoFloatsDebug(debug, "type=\"argos\"\n", sep="")
                      unlist(lapply(x[["argos"]], function(a) rep(a[[i]], length=length(a[["salinity"]]))))
                  } else if (type == "index") {
                      ## argoFloatsDebug(debug, "type=\"index\"\n", sep="")
                      x@data$index[[i]]
                  } else {
                      stop("[[\"", i, "\"]] only works for objects created by readProfiles()")
                  }
                  argoFloatsDebug(debug, "[[ for '", i, "' returning ", length(res), " values\n", sep="")
                  return(res)
              }
              return(NULL)
          })
