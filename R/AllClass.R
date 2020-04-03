## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' A Package for Collections of Argo Float Profiles
#'
#' This package **FIXME(jlh) Please write a few paragraphs here, after
#' discussions with dek and cgr.  The idea will be to get a clean
#' introductory paragraph that will organize our intentions for
#' the object structure.  Details ought to be added later, as
#' new things get written.  The oce package may provide a useful
#' guide.**
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
#' data(index, package="argoFloats")
#' plot(index, which="map")
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
#' data(indexBgc, package="argoFloats")
#' plot(indexBgc, which="map")
#' # Tabulate BGC parameters in these files
#' table(indexBgc[["parameters"]])
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
#' data(indexMerged, package="argoFloats")
#' plot(indexMerged, which="map")
#' table(indexMerged[["parameters"]])
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
#' 1. If `i=="metadata"` then the `metadata` slot of `x` is returned.
#'
#' 2. If `i=="data"` then the `data` slot of `x` is returned.
#'
#' 3. If `i=="ftpRoot"` and the object was created with [getIndex()],
#' then then the value of `ftpRoot` in the `metadata`
#' slot is returned.  (This is used by [getProfiles()] to construct
#' a vector of URLs to download.)
#'
#' 4. If `i=="destdir"` then the `destdir` item in the `metadata` slot is
#' returned. This is used by [getProfiles()] to decide where to
#' save downloaded files, and later by [readProfiles()], which
#' works with the output from [getProfiles()].
#'
#' 5. If `i=="type"` then the `type` item in the `metadata` slot is
#' returned. This is `"index"` if the object was created with
#' [getIndex()], `"profiles"` if it was created with [getProfiles()],
#' and `"argos"` if it was created with [readProfiles()].
#'
#' 6. If `i=="profile"` and `x` was created with [readProfiles()],
#' then the return value depends on the value of `j`. There are 4
#' sub-cases.  (a) If `j` is not supplied, the return values is a list containing
#' all the profiles in `x`, each an `argo` object as created by
#' [oce::read.argo()] in the `oce` package.  (b) If `j` is a single integer,
#' then the return value is a single `argo` object. (c) If `j` is a vector
#' of integers, then a list of `argo` objects is returned. (d) If `j` is
#' the string `"count"`, then the number of profiles is returned.
#'
## 7. If `i` is a single integer, then it is taken as an index to
## the most important element of the `data` slot of `x`. If `x` was
## created with [getIndex()], then that element is a file name on the
## remote Argo server, and that is what is returned.  If `x`
## was created with [getProfiles()], then the name of the local file
## is returned.  And if `x` was created with [readProfiles()], then the
## an `argo` object (as created with [oce::read.argo()]
## in the `oce` package) is returned.
##
## 7. If `i` is a vector of integers, then a vector is returned, with
## each element being as defined for case 7.
#'
#' 7. Otherwise, if `x` was created with [getIndex()],
#' `i` may be the name of an item in the `index` item
#' in the `data` slot, or a string that is made up of enough characters
#' to uniquely identify such an item, e.g. `"lon"` may be used as a
#' shortcut for `"longitude"`.
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
#' paste(index[["ftpRoot"]], index[["profile", 1:2]], sep="/")
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
#' @rdname argoFloats-methods
setMethod(f="[[",
          signature(x="argoFloats", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (missing(i))
                  stop("Must name an item to retrieve, e.g. 'x[[\"latitude\"]]'", call.=FALSE)
              type <- x@metadata$type
              if (length(i) != 1)
                  stop("length of index, 'i', must be one")
              if (i == "data")
                  return(x@data)
              if (i == "metadata")
                  return(x@metadata)
              if (i == "ftpRoot" && type == "index")
                  return(x@metadata$ftpRoot)
              if (i == "destdir")
                  return(x@metadata$destdir)
              if (i == "type")
                  return(x@metadata$type)
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

