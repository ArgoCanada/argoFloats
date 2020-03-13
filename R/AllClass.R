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
#' 3. If `i=="ftpRoot"` then the value of `ftpRoot` in the `metadata`
#' 5. If `i=="destdir"` then the `destdir` item in the `metadata` slot is
#' returned. This is used by [getProfiles()] to decide where to
#' save downloaded files, and later by [readProfiles()], which
#' works with the output from [getProfiles()].
#' 6. If `i=="type"` then the `type` item in the `metadata` slot is
#' returned. This is `"index"` if the object was created with
#' [getIndex()], `"profiles"` if it was created with [getProfiles()],
#' and `"argos"` if it ws created with [readProfiles()].
#' 7. If `i=="profile"` and the object `type` is `"argos"`, as will
#' be the case if `x` is a return value from [readProfiles()], return
#' either a list containing all profiles, or a requested profile;
#' see [readProfiles()] for examples.
#' 8. If `i=="profile count"` and the object `type` is `"argos"`
#' then the number of profiles is returned.
#' 8. Otherwise, `i` must be the name of an item in the `index` item
#' in the `data` slot, or a string that is made up of enough characters
#' to uniquely identify such an item, e.g. `"lon"` may be used as a
#' shortcut for `"longitude"`.
#'
#' @param x an [argoFloats-class] object.
#' @param i a character value that specifies the item to be looked up;
#' see \dQuote{Details}.
#' @param j ignored.
#' @param ... ignored.
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
              if (i == "data") {
                  return(x@data)
              } else if (i == "metadata") {
                  return(x@metadata)
              } else if (i == "ftpRoot") {
                  return(x@metadata$ftpRoot)
              } else if (i == "destdir") {
                  return(x@metadata$destdir)
              } else if (i == "type") {
                  return(x@metadata$type)
              } else if (i == "profile" && x@metadata$type == "argos") {
                  if (missing(j))
                      return(x@data$argos)
                  if (any(j < 0))
                      stop("cannot handle a negative index")
                  nargos <- length(x@data$argos)
                  if (any(j > nargos))
                      stop("cannot handle an index exceeding ", nargos)
                  if (length(j) == 1) {
                      return(x@data$argos[[j]])
                  } else {
                      res <- vector("list", length(j))
                      for (jj in j)
                          res[jj] <- x@data$argos[jj]
                      return(res)
                  }
              } else if (i == "profile count" && x@metadata$type == "argos") {
                  return(length(x@data$argos))
              } else if (i == "index") {
                  if (x@metadata$type == "index") {
                      return(x@data$index)
                  } else {
                      stop("can only retrieve 'index' for objects created by getIndex()")
                  }
              } else if (x@metadata$type == "index") {
                  names <- names(x@data$index)
                  w <- pmatch(i, names)
                  if (!is.finite(w))
                      stop("Unknown item '", i, "'; must be one of: '", paste(names, collapse="', '"), "'", call.=FALSE)
                  return(x@data$index[[names[w]]])
              }
              return(NULL)
          })




#' Summarize an argoFloats Object
#'
#' Show some key facts about an [argoFloats-class] object.
#'
#' @param object an [argoFloats-class] object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom oce processingLogShow
#' @importFrom methods callNextMethod
#' @importFrom utils head
#' @export
#'
#' @author Dan Kelley
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
#' Isolate a subset of an [argoFloats-class] object of type `"index"`,
#' as created by [getIndex()], either by specifying
#' indices to keep (using the `subset` argument) or by specifying
#' a way to determine those indices (using the `...` argument).
#'
#' The possibilities for the `...` argument are as follows.
#' 1. A list named `circle` with elements named `longitude`,
#' `latitude` and `radius`.  The first two give the center of
#' the subset region, and the third gives the radius of
#' that region, in kilometers.
#' 2. A list named `rectangle`, which has an element named `longitude`
#' with two elements giving the western and eastern limits of the
#' subset region, and similar one named `latitude` giving the southern
#' and northern limits.
#'
#' In both cases, the notation is that longitude is positive
#' for degrees East and negative for degrees West, and that latitude
#' is positive for degrees North and negative for degrees South.
#'
#' @param x an [argoFloats-class] object as created by [getIndex()].
#'
#' @param subset optional numerical or logical vector that indicates which
#' indices of `x@data$index` to keep.  See example 1.
#'
#' @param ... a list named `circle` or `rectangle`. See \dQuote{Details}
#' and Examples 2 and 3.
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
#' # Example 2: Profiles within 200km of Sable Island
#' aiSable <- subset(ai, circle=list(longitude=-59.915, latitude=44.934, radius=200))
#' cat("Found", length(aiSable[["longitude"]]), "profiles near Sable Island\n")
#'
#' # Example 3: Profiles in a given rectangle
#' library(oce)
#' data(coastlineWorldFine, package="ocedata")
#' aiRect <- subset(ai, rectangle=list(longitude=c(-65,-64), latitude=c(40,45)))
#' lat <- aiRect[['latitude']]
#' lon <- aiRect[['longitude']]
#' latlim <- c(40,43)
#' lonlim<- c(-70,-64)
#' mapPlot(coastlineWorldFine, col='lightgray', longitudelim=lonlim, latitudelim=latlim,
#'         projection="+proj=merc", grid=2)
#' mapPoints(lon,lat)
#' }
#'
#' @author Dan Kelley and Jaimie Harbin
#'
#' @importFrom oce geodDist
#' @export
setMethod(f="subset",
          signature="argoFloats",
          definition=function(x, subset=NULL, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              dots <- list(...)
              dotsNames <- names(dots)
              if (missing(subset)) {
                  if (length(dots) == 0)
                      stop("must specify the subset, with 'subset' argument,'circle', or 'rectangle'")
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
                          stop("In subset,argoFloats-method() : 'circle' must be a list containing 'longitude', 'latitude' and 'radius'.")
                      if (3 != sum(c("longitude", "latitude", "radius") %in% sort(names(circle))))
                          stop("In subset,argoFloats-method() : 'circle' must be a list containing 'longitude', 'latitude' and 'radius'")
                      dist <- geodDist(x[["longitude"]], x[["latitude"]], circle$longitude, circle$latitude)
                      keep <- dist < circle$radius
                      keep[is.na(keep)] <- FALSE
                      if (sum(keep) < 1)
                          warning("In subset,argoFloats-method(..., circle) : found no profiles within ", circle$radius, "km of ", circle$longitude, "E and ", circle$latitude, "N\n", call.=FALSE)
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1] == "rectangle") {
                      rectangle <- dots[[1]]
                      if (!is.list(dots[1]))
                          stop("In subset,argoFloats-method() : 'rectangle' must be a list containing 'longitude' and 'latitude'")
                      if (2 != sum(c("longitude", "latitude") %in% sort(names(rectangle))))
                          stop("In subset,argoFloats-method() : 'rectangle' must be a list containing 'longitude' and 'latitude'")
                      keeplon <- rectangle$longitude[1] <=x[["longitude"]] & x[["longitude"]] <= rectangle$longitude[2]
                      keeplat <- rectangle$latitude[1] <= x[["latitude"]] & x[['latitude']] <= rectangle$latitude[2]
                      keeplon[is.na(keeplon)] <- FALSE
                      if (sum(keeplon) < 1)
                          warning("In subset,argoFloats-method(..., rectangle) : found no profiles between given longitudes", call.=FALSE)
                      keeplat[is.na(keeplat)] <- FALSE
                      if (sum(keeplat) < 1)
                          warning("In subset,argoFloats-method(..., rectangle) : found no profiles between given latitudes", call.=FALSE)
                      x@data$index <- x@data$index[keeplon&keeplat, ]
                  } else {  
                      stop("In subset,argoFloats-method() : the only permitted '...' argument is a list named 'circle' or 'rectangle'", call.=FALSE)
                  }
              } else {
                  if (length(dotsNames) != 0)
                      stop("in subset,argoFloats-method() : cannot give both 'subset' and '...' arguments", call.=FALSE)
                  if (x@metadata$type == "index") {
                      x@data$index <- x@data$index[subset, ]
                  } else {
                      stop("In subset,argoFloats-method() : method not coded except for type=\"index\"", call.=FALSE)
                  }
              }
              x
          })


