## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

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
                      message("Kept ", sum(keep), " profiles (", sprintf("%.2g", 100*sum(keep)/length(keep)), "%)")
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
                      message("Kept ", sum(keep), " profiles (", sprintf("%.2g", 100*sum(keep)/length(keep)), "%)")
                  } else if (dotsNames[1]=="parameter") {
                      parameter <- dots[[1]]
                      if (is.list(dots[1]))
                          parameters <- unlist(parameter)
                      nparameters <- length(parameters)
                      parametersList <- lapply(x[["parameters"]], function(p) strsplit(p, " ")[[1]])
                      keep <- unlist(lapply(parametersList, function(pl) nparameters == sum(parameters %in% pl)))
                      if (sum(keep) < 1)
                          warning("In subset,argoFloats-method(..., parameter) : found no profiles with given parameter", call.=FALSE)
                      message("Kept ", sum(keep), " profiles (", sprintf("%.2g", 100*sum(keep)/length(keep)), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1]=="polygon") {
                      polygon <- dots[[1]]
                      if(!is.list(dots[1]))
                          stop("In subset,argoFloats-method() : 'polygon' must be a list")
                      keep <- as.logical(sp::point.in.polygon(x[["longitude"]], x[["latitude"]],
                                                              polygon$longitude, polygon$latitude))
                      keep[is.na(keep)] <- FALSE
                      message("Kept ", sum(keep), " profiles (", sprintf("%.2g", 100*sum(keep)/length(keep)), "%)")
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
                      keep <- time$from[1] <= x[["date"]] & x[["date"]] <= time$to[1]
                      keep[is.na(keep)] <- FALSE
                      message("Kept ", sum(keep), " profiles (", sprintf("%.2g", 100*sum(keep)/length(keep)), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if(dotsNames[1]=="institution") {
                      institution <- dots[[1]]
                      if(!is.list(dots[1]))
                          stop("In subset,argoFloats-method() : 'institution' must be a list")
                      keep <- grepl(institution, x@data$index$institution)
                      keep[is.na(keep)] <- FALSE
                      message("Kept ", sum(keep), " profiles (", sprintf("%.2g", 100*sum(keep)/length(keep)), "%)")
                      x@data$index <- x@data$index[keep, ]
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
