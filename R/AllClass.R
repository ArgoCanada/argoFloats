    # vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' A Package for Processing Argo Float Profiles
#'
#' The `argoFloats` package provides tools for downloading and processing Argo profile data.
#' It allows users to focus on core argo, biogeochemical (bgc) argo, or deep argo profiles, and
#' also to sift these profiles based on ID, time, geography, variable, institution, and ocean.
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
#' 2. `plot()` provides simple ways to plot aspects of [argoFloats-class] objects.
#' See [plot,argoFloats-method()] for details.
#'
#' 3. `summary()` displays key features of [argoFloats-class] objects.
#' See [summary,argoFloats-method()] for details.
#'
#' 4. `show()` provides a one-line sketch of [argoFloats-class] objects.
#' See [show,argoFloats-method()] for details.
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
#' This is created by subsetting a global index to the 1788 Argo profiles
#' that were within a 200km radius of Marsh Harbour, Abaco Island,
#' Bahamas, as of 2020 March 14, using
#' the following code.
#'\preformatted{
#  library(argoFloats)
#' library(oce)
#' indexAll <- getIndex()
#' index <- subset(indexAll,
#'     circle=list(longitude=-77.06,latitude=26.54,radius=200))
#'}
#'
#' @examples
#' library(argoFloats)
#' data(index)
#' plot(index)
#'
#' @name index
#'
#' @docType data
#'
#' @family datasets provided with argoFloats
NULL


#' A sample index of biogeochemical-argo profiles
#'
#' This is created by subsetting a global index to the 39 BGC Argo profiles
#' that were within a 300km radius of Marsh Harbour, Abaco Island,
#' Bahamas, as of 2020 March 23, using
#' the following code.
#'\preformatted{
#' library(argoFloats)
#' indexAll <- getIndex(file="bgc")
#' indexBgc <- subset(indexAll, circle=list(longitude=-77.06, latitude=26.54, radius=300))
#' save(indexBgc, file="indexBgc.rda")
#' tools::resaveRdaFiles('indexBgc.rda')
#'}
#'
#' @examples
#' library(argoFloats)
#' data(indexBgc)
#' plot(indexBgc)
#' summary(indexBgc)
#' unique(indexBgc[["parameters"]])
#'
#' @name indexBgc
#'
#' @docType data
#'
#' @family datasets provided with argoFloats
NULL


#' A sample index of merged argo and biogeochemical-argo profiles
#'
#' This is created by subsetting a global index to the 39 BGC Argo profiles
#' that were within a 300km radius of Marsh Harbour, Abaco Island,
#' Bahamas, as of 2020 March 23, using
#' the following code.
#'\preformatted{
#' library(argoFloats)
#' indexAll <- getIndex(file="merged")
#' indexMerged <- subset(indexAll, circle=list(longitude=-77.06, latitude=26.54, radius=300))
#' save(indexMerged, file="indexMerged.rda")
#' tools::resaveRdaFiles('indexMerged.rda')
#'}
#'
#' @examples
#' library(argoFloats)
#' data(indexMerged)
#' plot(indexMerged)
#' summary(indexMerged)
#' unique(indexMerged[["parameters"]])
#'
#' @name indexMerged
#'
#' @docType data
#'
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
#' This function provides an easy way to look up values within an [argoFloats-class]
#' object, without the need to know the exact structure of the data. The action
#' taken by `[[` depends on the `type` element in the `metadata` slot of
#' the object (which is set by the function that created the object), on the
#' value of `i` and, in some cases, on the value of `j`; see \dQuote{Details}.
#'
#' There are several possibilities, depending on the object `type`, as
#' follows.
#'
#' 1. For all object types:
#'     1. If `i` is `"metadata"` then the `metadata` slot of `x` is returned.
#'     2. If `i` is `"data"` then the `data` slot of `x` is returned.
#'
#' 2. If `type` is `"index"`, i.e. if `x` was created with [getIndex()]
#' or with [subset,argoFloats-method()] acting on the result of [getIndex()])),
#' then:
#'     1. If `i` is the name of an item in the `metadata` slot, then that item
#'        is returned. The choices are:
#'        `"destdir"`, `"destfileRda"`, `"filename"`, `"ftpRoot"`, `"header"`,
#'        `"server"`, `"type"`, and `"url"`.
#'     2. If `i` is the name of an item in the `data` slot, then that item
#'        is returned.  The choices are:
#'        `"date"`, `"date_update"`, `"file"`, `"institution"`, `"latitude"`,
#'        `"longitude"`, `"ocean"`, and `"profiler_type"`.
#'     3. If `i=="index"` then that item from the `data` slot of `x` is returned.
#'        (For the possible names, see the previous item in this sublist.)
#'     4. If `i` is an integer, then the `i`-th row of the `index` item in
#'        the `data` slot is returned.  This is a good way to learn the
#'        longitude and latitude of the profile, the file name on the server,
#'        etc.
#'     4. If `i` is `"profile"` then the return value is developed from the
#'        `index$file` item within the `data` slot of `x`, in one of three cases:
#'         1. If `j` is not supplied, the return value is the full contents
#'            of `file`, i.e. the full names of the files downloaded
#'            by [getProfiles()].
#'         2. If `j` is numeric, then the return value is a vector of the
#'            `file` entries, as indexed by `j`.
#'         3. If `j` is `"count"`, then the number of `file` names is returned.
##' 3. If `type` is `"profiles"`, i.e. if `x` was created with [getProfiles()], then:
#'     1. If `i` is the name of an item in the `metadata` slot, then that item
#'        is returned. The choices are:
#'        `"type"` and `"destdir"`.
#'     2. If `i` is the name of an item in the `data` slot, then that item
#'        is returned.  There is only one choice: `"file"`.
#'     3. If `i` is `"profile"` then the return value is developed from the
#'        `file` item in the `data` slot of `x`, in one of three cases:
#'         1. If `j` is not supplied, the return value is `file`,
#'            i.e. a vector holding the full names of the files downloaded
#'            by [getProfiles()].
#'         2. If `j` is numeric, then the return value is a vector of the
#'            `file` entries, as indexed by `j`.
#'         3. If `j` is `"count"`, then the number of `file` names is returned.
#' 4. If `type` is `"argos"`, i.e. if `x` was created with [readProfiles()], then:
#'     1. If `i` is the name of an item in the `metadata` slot, then that item
#'        is returned. There is only choice, `"type"`.
#'     2. If `i` is the name of an item in the `data` slot, then that item
#'        is returned.  There is only one choice: `"argos"`.
#'     3. If `i` is `"profile"` then the return value depends on the value of `j`.
#'         There are four sub-cases.
#'         1. If `j` is not supplied, the return valuesis a list containing
#'             all the profiles in `x`, each an `argo` object as created by
#'             [oce::read.argo()] in the `oce` package.
#'         2. If `j` is a single integer,  then the return value is a single
#'            `argo` object.
#'         3. If `j` is a vector  of integers, then a list of `argo` objects
#'            is returned.
#'         4. If `j` is `"count"`, then the number of profiles is returned.
#'
#' @param x an [argoFloats-class] object.
#' @param i a character value that specifies the item to be looked up;
#' see \dQuote{Details}.
#' @param j supplemental index value, ignored unless `i` is `"profile"` (see \dQuote{Details}).
#' @param ... ignored.
#'
#' @examples
#' data(index)
#' # Full remote filename for first two item in index
#' paste0(index[["server"]], "/dac/", index[["profile", 1:2]])
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
## @rdname argoFloats-methods
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
                  } else if (length(i) == 1 && is.numeric(i)) {
                      return(x@data$index[i,])
                  } else if (length(i) == 1 && i %in% names(x@metadata)) {
                      return(x@metadata[[i]])
                  } else if (length(i) == 1 && i %in% names(x@data$index)) {
                      return(x@data$index[[i]])
                  } else if (length(i) == 1 && i == "profile") {
                      if (missing(j)) {
                          return(x@data$index$file)
                      } else if (length(j) == 1 && j[1] == "count") {
                          return(length(x@data$index$file))
                      } else if (is.numeric(j)) {
                          return(x@data$index$file[j])
                      } else {
                          stop("cannot interpret i=", paste(i, collapse=","), " and j=", paste(j, collapse=", "), " for an object of type=\"", type, "\"")
                      }
                  } else {
                      stop("cannot interpret i=", paste(i, collapse=","), " for an object of type=\"index\"")
                  }
              } else if (type == "profiles") { # made by getProfiles()
                  if (length(i) == 1 && i %in% names(x@metadata)) {
                      return(x@metadata[[i]])
                  } else if (length(i) == 1 && i %in% names(x@data)) {
                      return(x@data[[i]])
                  } else if (length(i) == 1 && i == "profile") {
                      if (missing(j)) {
                          return(x@data$file)
                      } else if (length(j) == 1 && j[1] == "count") {
                          return(length(x@data$file))
                      } else if (is.numeric(j)) {
                          return(x@data$file[j])
                      } else {
                          stop("cannot interpret i=", paste(i, collapse=","), " and j=", paste(j, collapse=", "), " for an object of type=\"", type, "\"")
                      }
                  } else {
                      stop("cannot interpret i=", paste(i, collapse=","), " for an object of type=\"", type, "\"")
                  }
              } else if (type == "argos") { # made by readProfiles()
                  if (length(i) == 1 && i %in% names(x@metadata)) {
                      return(x@metadata[[i]])
                  } else if (length(i) == 1 && i %in% names(x@data)) {
                      return(x@data[[i]])
                  } else if (length(i) == 1 && i == "profile") {
                      if (missing(j)) {
                          return(x@data$argos)
                      } else if (length(j) == 1 && j[1] == "count") {
                          return(length(x@data$argos))
                      } else if (is.numeric(j)) {
                          return(x@data$argos[[j]])
                      } else {
                          stop("cannot interpret i=", paste(i, collapse=","), " and j=", paste(j, collapse=", "), " for an object of type=\"", type, "\"")
                          stop("FIXME: err here")
                      }
                   } else {
                      stop('FIXME: look within objects (and document)')
                  }
              } else {
                  stop("the object type must be \"index\", \"profiles\", or \"argos\", but it is \"", type, "\"")
              }
              ## FIXME: move useful things from the below to the above, and then delete the below.
              if (i == "profile") {
                  ## if j not given, return full list (either 'file' or 'argos')
                  if (missing(j)) {
                      return(switch(type,
                                    index=length(x@data$index$file),
                                    profile=length(x@data$file),
                                    argo=length(x@data$argos)))
                  }
                  if (any(j < 1))
                      stop("index, 'j', must be positive")
                  n <- switch(type,
                              index=length(x@data$index$file),
                              profile=length(x@data$file),
                              argo=length(x@data$argos))
                  if (any(j > n))
                      stop("index, j', cannot exceed ", n)
                  if (length(j) == 1) {
                      return(switch(type,
                                    index=x@data$index$file[j],
                                    profile=x@data$file[j],
                                    argos=x@data$argos[[j]],
                                    stop("unknown object type '", type, "': coding error (please report)")))
                  }
                  if (type == "index") {
                      res <- x@data$index$file[j]
                  } else if (type == "profile") {
                      res <- x@data$file[j]
                  } else if (type == "argo") {
                      res <- vector("list", length(j))
                      for (jj in j)
                          res[jj] <- x@data$argos[jj]
                  } else {
                      stop("type='", type, "' is not handled (please report as a bug)")
                  }
                  return(res)
              }
              if (i == "argos") {
                  if (type != "argos")
                      stop("[[\"argos\"]] only works for objects created by readProfiles()")
                  return(x@data$argos)
              }
              if (i %in% c("salinity", "temperature", "pressure")) {
                  if (type != "argos")
                      stop("[[\"", i, "\"]] only works for objects created by readProfiles()")
                  res <- unlist(lapply(x[["argos"]], function(a) a[[i]]))
                  argoFloatsDebug(debug, "[[ for '", i, "' returning ", length(res), " values\n", sep="")
                  return(res)
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
              if (i == "profile count") {
                  return(switch(type,
                                index=length(x@data$index$file),
                                profile=length(x@data$file),
                                argo=length(x@data$argos)))
              }
              if (i == "index" && type == "index")
                  return(x@data$index)
              if (x@metadata$type == "index") {
                  names <- names(x@data$index)
                  w <- pmatch(i, names)
                  if (!is.finite(w))
                      stop("Unknown item '", i, "'; must be one of: '", paste(names, collapse="', '"), "'", call.=FALSE)
                  return(x@data$index[[names[w]]])
              }
              ## The request made no sense, so return NULL.
              return(NULL)
          })

