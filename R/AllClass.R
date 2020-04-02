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
#' that region, in kilometers. See Example 2A.
#' 2. A list named `rectangle`, which has an element named `longitude`
#' with two elements giving the western and eastern limits of the
#' subset region, and similar one named `latitude` giving the southern
#' and northern limits. See Example 2B.
#' 3. A list named `polygon` that has elements named `longitude` and `latitude`.
#' Profiles within this polygon will be retained. See Example 2C.
#' 4. A vector or list named `parameter` that holds names of parameters to keep;
#' see examples 3A and 3B.
#' 5. A list named `time` that has elements `from` and `to` that are POSIXt
#' times created with eg. [POSIXct()], with `tz="UTC"` to match
#' the timezone used in Argo data. Profiles within that time frame will
#' be retained.
#' 6. A list named `institution`, which has `argo`, `argo_bgc`, or `argo_merge`
#' institutions (ie. `AO` `BO` `CS` `HZ` `IF` `IN` `JA` `KM` `KO` `ME` `NM`). 
#'
#' In all cases, the notation is that longitude is positive
#' for degrees East and negative for degrees West, and that latitude
#' is positive for degrees North and negative for degrees South.
#'
#' @param x an [argoFloats-class] object as created by [getIndex()].
#'
#' @param subset optional numerical or logical vector that indicates which
#' indices of `x@data$index` to keep.  See example 1.
#'
#' @param ... a list named `circle`, `rectangle`, `parameter`, `polygon` , or `time`. See \dQuote{Details}
#' and \dQuote{Examples}.
#'
#' @return An [argoFloats-class] object.
#'
#' @examples
#' library(argoFloats)
#' data(index)
#'
#' # Example 1: First three profiles in dataset.
#' index1 <- subset(index, 1:3)
#' cat("First 3 longitudes:", paste(index1[["longitude"]]), "\n")
#'
#' # Example 2: Demonstrate geographical subsets. These examples use
#' # built-in datasets; to use these in other  work, start by
#' # using getIndex() to create an index.
#' # 2A: circle north of Abaca Island
#' index2A <- subset(index, circle=list(longitude=-77.5, latitude=27.5, radius=50))
#' # 2B: rectangle northeast of Abaca Island
#' lonRect <- c(-76.5, -76)
#' latRect <- c(26.5, 27.5)
#' index2B <- subset(index, rectangle=list(longitude=lonRect, latitude=latRect))
#' # 2C: polygon to the southeast of Abaco Island
#' lonPoly <- c(-76.5, -76.0, -75.5)
#' latPoly <- c(25.5, 26.5, 25.5)
#' index2C <- subset(index, polygon=list(longitude=lonPoly, latitude=latPoly))
#' # Show these subsets on a map
#' plot(index)
#' points(index2A[["longitude"]], index2A[["latitude"]], col=2, pch=20, cex=1.4)
#' points(index2B[["longitude"]], index2B[["latitude"]], col=3, pch=20, cex=1.4)
#' rect(lonRect[1], latRect[1], lonRect[2], latRect[2], border=3, lwd=2)
#' points(index2C[["longitude"]], index2C[["latitude"]], col=4, pch=20, cex=1.4)
#' polygon(lonPoly, latPoly, border=4, lwd=2)
#'
#' # Example 3: Subsetting argo_merge data containing 'DOXY' parameters
#' # 3A: Data containing 'DOXY' data
#' data(indexMerged)
#' index3A <- subset(indexMerged, parameter="DOXY")
#' # 3B: Data containing both 'PSAL' and 'DOWN_IRRADIANCE380' data
#' data(indexMerged)
#' index3B <- subset(indexMerged, parameter=c("PSAL", "DOWN_IRRADIANCE380"))
#'
#' # Example 4: Subsetting data for the year 2019
#' data(index)
#' from <- as.POSIXct("2019-01-01", tz="UTC")
#' to <- as.POSIXct("2019-12-31", tz="UTC")
#' index4 <- subset(index, time=list(from=from, to=to))
#'
#' @author Dan Kelley and Jaimie Harbin
#'
#' @importFrom oce geodDist
#' @importFrom sp point.in.polygon
#' @export
setMethod(f="subset",
          signature="argoFloats",
          definition=function(x, subset=NULL, ...) {
              ##subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              dots <- list(...)
              dotsNames <- names(dots)
              if (missing(subset)) {
                  if (length(dots) == 0)
                      stop("must specify the subset, with 'subset' argument,'circle','rectangle', 'parameter','polygon', 'time', or 'institution'")
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
                      x@data$index <- x@data$index[keep, ]
                      message("Kept ", sum(keep), " profiles (", round(100*sum(keep)/length(keep),2), "%)")
                  } else if (dotsNames[1] == "rectangle") {
                      rectangle <- dots[[1]]
                      if (!is.list(dots[1]))
                          stop("In subset,argoFloats-method() : 'rectangle' must be a list containing 'longitude' and 'latitude'")
                      if (2 != sum(c("longitude", "latitude") %in% sort(names(rectangle))))
                          stop("In subset,argoFloats-method() : 'rectangle' must be a list containing 'longitude' and 'latitude'")
                      keeplon <- rectangle$longitude[1] <=x[["longitude"]] & x[["longitude"]] <= rectangle$longitude[2]
                      keeplat <- rectangle$latitude[1] <= x[["latitude"]] & x[['latitude']] <= rectangle$latitude[2]
                      ok <- is.finite(keeplon) & is.finite(keeplat)
                      keeplon[!ok] <- FALSE
                      keeplat[!ok] <- FALSE
                      keep <- keeplon & keeplat
                      x@data$index <- x@data$index[keep, ]
                      message("Kept ", sum(keep), " profiles (", round(100*sum(keep)/length(keep),2), "%)")
                  } else if (dotsNames[1]=="parameter") {
                      parameter <- dots[[1]]
                      if (is.list(dots[1]))
                          parameters <- unlist(parameter)
                      nparameters <- length(parameters)
                      parametersList <- lapply(x[["parameters"]], function(p) strsplit(p, " ")[[1]])
                      keep <- unlist(lapply(parametersList, function(pl) nparameters == sum(parameters %in% pl)))
                      if (sum(keep) < 1)
                          warning("In subset,argoFloats-method(..., parameter) : found no profiles with given parameter", call.=FALSE)
                      message("Kept ", sum(keep), " profiles (", round(100*sum(keep)/length(keep),2), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1]=="polygon") {
                      polygon <- dots[[1]]
                      if(!is.list(dots[1]))
                          stop("In subset,argoFloats-method() : 'polygon' must be a list")
                      keep <- as.logical(sp::point.in.polygon(x[["longitude"]], x[["latitude"]],
                                                              polygon$longitude, polygon$latitude))
                      message("Kept ", sum(keep), " profiles (", round(100*sum(keep)/length(keep),2), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1]=="time") {
                      time <- dots[[1]]
                      if(!is.list(dots[1]))
                          stop("In subset,argoFloats-method() : 'time' must be a list")
                      if (!inherits(time$from, "POSIXt"))
                          stop("'time' must be a list containing POSIX times")
                      if (2 != sum(c("from", "to") %in% names(time)))
                          stop("In subset,argoFloats-method() : 'time' must be a list containing 'to'and 'from'")
                      if (length(time$from) != 1)
                          stop("from must be of length 1")
                      if (length(time$to) != 1)
                         stop("to must be of length 1")
                      if (time$to <= time$from)
                         stop ("'to' must be greater than 'from'")
                     keeptime <- time$from[1] <= x[["date"]] & x[["date"]] <= time$to[1]
                     keeptime[is.na(keeptime)] <- FALSE
                     #browser()
                     if (sum(keeptime) < 1)
                         warning("In subset,argoFloats-method(..., time) : found no profiles within the given time frame", call.=FALSE)
                     message("Fraction kept ", round(100*sum(keeptime)/length(keeptime),2), "%.")
                     x@data$index <- x@data$index[keeptime, ]
                } else if(dotsNames[1]=="institution") {
                    institution <- dots[[1]]
                    if(!is.list(dots[1]))
                        stop("In subset,argoFloats-method() : 'institution' must be a list")
                    keepinst <- grepl(institution, x@data$index$institution)
                    if (sum(keepinst) < 1)
                        warning("In subset,argoFloats-method(..., institution) : found no profiles from given institution", call.=FALSE)
                    message("Fraction kept ", round(100*sum(keepinst)/length(keepinst),2), "%.")
                    x@data$index <- x@data$index[keepinst, ]
                  } else {
                      stop("In subset,argoFloats-method() : the only permitted '...' argument is a list named 'circle','rectangle','parameter','polygon', 'time', or 'institution'", call.=FALSE)
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
