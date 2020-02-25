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
#' @import knitr
## @importFrom oce subset summary
#' @name argoFloats-package
#' @docType package
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
#' object, without the need to know the exact structure of the data. There are three
#' modes of operation, as determined by the `i` argument.
#'
#' The possibilities for the value of `i` are as follows.
#' 1. If `i=="metadata"` then the `metadata` slot is returned.
#' 2. If `i=="data"` then the `data` slot is returned.
#' 3. If `i=="ftpRoot"` then the value of `ftpRoot` in the `metadata`
#' slot is returned.  This is used by [getProfiles()] to construct
#' a vector of URLs to download.
#' 4. If `i=="index"` then the `index` item in the `data` slot is
#' returned. This can be useful in trying to understand more about
#' the profiles, although the informaton provided is somewhat limited.
#' 5. Otherwise, `i` must be the name of an item in the `index` item
#' in the `data` slot, or a string that is made up of enough characters
#' to uniquely identify such an item, e.g. `"lon"` may be used as a
#' shortcut for `"longitude"`.
#'
#' @param x a [argoFloats-class] object.
#' @param i a character value that specifies the item to be looked up;
#' see \dQuote{Details}.
#' @param j ignored.
#' @param ... ignored.
#'
#' @author Dan Kelley
#'
#' @export
setMethod(f="[[",
          signature(x="argoFloats", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (missing(i))
                  stop("Must name an item to retrieve, e.g. 'x[[\"latitude\"]]'", call.=FALSE)
              if (x@metadata$type == "index") {
                  if (i == "data") {
                      return(x@data)
                  } else if (i == "metadata") {
                      return(x@metadata)
                  } else if (i == "ftpRoot") {
                      return(x@metadata$ftpRoot)
                  } else if (i == "index") {
                      return(x@data$index)
                  } else {
                      names <- names(x@data$index)
                      w <- pmatch(i, names)
                      if (!is.finite(w))
                          stop("Unknown item '", i, "'; must be one of: '", paste(names, collapse="', '"), "'", call.=FALSE)
                      return(x@data$index[[names[w]]])
                  }
              } else {
                  stop("only for type 'index'")
              }
          })




#' Summarize an argoFloats Object
#'
#' @param object a [argoFloats-class] object.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom oce processingLogShow
#' @importFrom methods callNextMethod
#' @importFrom utils head
#'
#' @export
#'
#' @aliases summary.argoFloats
setMethod(f="summary",
          signature="argoFloats",
          definition=function(object, ...)
          {
              cat("argoFloats summary\n------------------\n\n")
              cat("* type:        ", object@metadata$type, "\n", sep="")
              if (object@metadata$type == "index") {
                  with(object@metadata, {
                       cat("* server:      ", server, "\n", sep="")
                       cat("* file:        ", file, "\n", sep="")
                       cat("* url:         ", url, "\n", sep="")
                       cat("* ftpRoot:     ", ftpRoot, "\n", sep="")
                       cat("* destfileRda: ", destfileRda, "\n", sep="")
                       if (length(header)) {
                           cat("* header:\n", sep="")
                           for (h in header)
                               cat("    ", h, "'\n", sep="")
                       } else {
                           cat("* header: (none)\n", sep="")
                       }})
                  ndata <- length(object@data$index$file)
                  if (ndata > 0) {
                      cat("* index (each holding ", ndata, if (ndata>1) " elements):\n" else " element):\n", sep="")
                      for (name in names(object@data$index)) {
                          cat("    ", name, ": ",
                              paste(head(object@data$index[[name]], 3), collapse=", "),
                              if (ndata > 1) ", ...\n" else "\n", sep="")
                      }
                  } else {
                      cat("* index: (none)\n", sep="")
                  }
              } else if (object@metadata$type == "profiles") {
                  cat("* FIXME: add special info for 'profiles' type\n")
              }
              processingLogShow(object)
              invisible()
          }
          )


#' Subset an argoFloats Object
#'
#' Return a subset of an [argoFloats-class] object, either by specifying
#' indices to keep (using the `subset` argument) or by specifying
#' a way to determine those indices (using the `...` argument).
#'
#' At the moment, this method only works for [argoFloats-class] objects
#' of type `"index"`, as read by [getIndex()].
#' A future version will also handle `...` arguments named
#' `polygon`, `rectangle` and `time`.
#'
#' @param x an [argoFloats-class] object.
#'
#' @param subset optional numerical or logical vector that indicates which
#' indices of `x@data$index` to keep.  See example 1.
#'
#' @param ... a single named list. At the moment, the only possibility
#' is a list named `circle`, which holds elements named `longitude`,
#' `latitude` and `radius`.  The first two specify a location, and
#' the third species a search radius in kilometers, as in Example 2.
#'
#' @return An [argoFloats-class] object.
#'
#' @examples
#' library(argoFloats)
#'\dontrun{
#' ai <- getIndex(file="argo_bio-profile_index.txt.gz", destdir="~/data/argo")
#' # Example 1: First three profiles in dataset.
#' aiFirstThree <- subset(ai, 1:3)
#' cat("First three longitudes:", paste(aiFirstThree[["longitude"]]), "\n")
#'
#' # Example 2: Profiles within 150km of Sable Island
#' lon0 <- -59.9149
#' lat0 <- 43.9337
#' aiSable <- subset(ai, circle=list(longitude=lon0, latitude=lat0, radius=150))
#' cat("Found", length(aiSable[["longitude"]]), "profiles near Sable Island\n")
#'}
#'
#' @author Dan Kelley
#'
#' @aliases subset.argoFloats
#'
#' @importFrom oce geodDist
#' @export
setMethod(f="subset",
          signature="argoFloats",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              dots <- list(...)
              dotsNames <- names(dots)
              if (missing(subset)) {
                  if (length(dots) == 0)
                      stop("must specify the subset, with 'subset' argument or with 'circle'")
                  if (length(dots) > 1)
                      stop("in subset,argoFloats-method() : cannot give more than one method in the '...' argument", call.=FALSE)
                  ## FIXME: permit args 'polygon', 'rectangle', and 'time'.
                  ##
                  ## note the [1] and [[1]] in next few lines, since we may want to
                  ## permit multiple '...' elements in a future version, e.g. maybe
                  ## the user would like to subset by both location and by time.
                  if (dotsNames[1] == "circle") {
                      circle <- dots[[1]]
                      if (!is.list(dots[1]))
                          stop("in subset,argoFloats-method() : 'circle' must be a list containing 'longitude', 'latitude' and 'radius'.")
                      if (3 != sum(c("longitude", "latitude", "radius") %in% sort(names(circle))))
                          stop("in subset,argoFloats-method() : 'circle' must be a list containing 'longitude', 'latitude' and 'radius'")
                      dist <- geodDist(x[["longitude"]], x[["latitude"]], circle$longitude, circle$latitude)
                      keep <- dist < circle$radius
                      keep[is.na(keep)] <- FALSE
                      x@data$index <- x@data$index[keep, ]
                  } else {
                      stop("in subset,argoFloats-method() : the only permitted '...' argument is a list named 'circle'")
                  }
              } else {
                  if (length(dotsNames) != 0)
                      stop("in subset,argoFloats-method() : cannot give both 'subset' and '...' arguments", call.=FALSE)
                  if (x@metadata$type == "index") {
                      x@data$index <- x@data$index[subset, ]
                  } else {
                      stop("method not coded except for type=\"index\"")
                  }
              }
              x
          })


