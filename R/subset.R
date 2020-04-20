## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Subset an argoFloats Object
#'
#' Isolate a subset of an [argoFloats-class] object of type `"index"`,
#' as created by [getIndex()], either by specifying
#' indices to keep (using the `subset` argument) or by specifying
#' a way to determine those indices (using the `...` argument).
#'
#' The possibilities for the `...` argument are as follows.
#'
#' 1. An integer vector giving indices to keep. See example 1.
#'
#' 2. A list named `circle` with numeric elements named `longitude`,
#' `latitude` and `radius`.  The first two give the center of
#' the subset region, and the third gives the radius of
#' that region, in kilometers. See example 2.
#'
#' 3. A list named `rectangle`, which has elements named
#' `longitude` and `latitude`, two-element numeric vectors
#' giving the western and eastern, and southern and northern
#' limits of the selection region. See example 3.
#'
#' 4. A list named `polygon` that has elements named `longitude` and `latitude`
#' that are numeric vectors specifying a polygon within which profiles
#' will be retained. If the polygon is not closed (i.e. if the first and
#' last points do not coincide) then a warning is issued, and the first
#' point is pasted onto the end.  See example 4.
#'
#' 5. A vector or list named `parameter` that holds character values that
#' specify the names of measured parameters to keep. See example 5.
#'
#' 6. A list named `time` that has elements `from` and `to` that are POSIXt
#' times that were created with eg. [POSIXct()], with `tz="UTC"` to match
#' the timezone used in Argo data. Profiles within that time frame will
#' be retained. See example 6.
#'
#' 7. A list named `institution`, which holds a single character element that
#' names the institution.  The permitted values are:
#' `"AO"` for AOML, USA;
#' `"BO"` for BODC, United Kingdom;
#' `"CS"` for CSIRO, Australia;
#' `"HZ"` for CSIO, China Second Institute of Oceanography;
#' `"IF"` for Ifremer, France;
#' `"IN"` for INCOIS, India;
#' `"JA"` for JMA, Japan;
#' `"KM"` for KMA, Korea;
#' `"KO"` for KORDI, Korea;
#' `"ME"` for MEDS, Canada; and
#' `"NM"` for NMDIS, China.
#'
#' 8. A list named `deep` that holds a logical value indicating weather argo floats
#' are deep argo (ie. `profiler_type` 849, 862, and 864). See example 8.
#'
#' 9. A list named `ID` that holds a character value specifying a float identifier.
#' See example 9.
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
#' @param ... a list named `circle`, `rectangle`, `parameter`, `polygon` , or `time`.
#' See \dQuote{Details} and \dQuote{Examples}.
#'
#' @return An [argoFloats-class] object.
#'
#' @examples
#' library(argoFloats)
#' data(index)
#'
#' # Example 1: subset to the first 3 profiles in the (built-in) index
#' index1 <- subset(index, 1:3)
#' cat("First 3 longitudes:", paste(index1[["longitude"]]), "\n")
#'
#' # Example 2: subset to a circle near Abaca Island
#' index2 <- subset(index, circle=list(longitude=-77.5, latitude=27.5, radius=50))
#'
#' # Example 3: subset to a rectangle near Abaca Island
#' lonRect <- c(-76.5, -76)
#' latRect <- c(26.5, 27.5)
#' index3 <- subset(index, rectangle=list(longitude=lonRect, latitude=latRect))
#'
#' # Example 4: subset to a polygon to near Abaco Island
#' lonPoly <- c(-76.5, -76.0, -75.5)
#' latPoly <- c(25.5, 26.5, 25.5)
#' index4 <- subset(index, polygon=list(longitude=lonPoly, latitude=latPoly))
#'
#' # Show some of these subsets on a map
#' plot(index)
#' points(index2[["longitude"]], index2[["latitude"]], col=2, pch=20, cex=1.4)
#' points(index3[["longitude"]], index3[["latitude"]], col=3, pch=20, cex=1.4)
#' rect(lonRect[1], latRect[1], lonRect[2], latRect[2], border=3, lwd=2)
#' points(index4[["longitude"]], index4[["latitude"]], col="magenta", pch=20, cex=1.4)
#' polygon(lonPoly, latPoly, border="magenta", lwd=2)
#'
#' # Example 5: subset argo_merge data containing 'DOXY' parameters
#' # Data containing 'DOXY' data
#' data(indexMerged)
#' index5A <- subset(indexMerged, parameter="DOXY")
#' # Data containing both 'PSAL' and 'DOWN_IRRADIANCE380' data
#' data(indexMerged)
#' index5B <- subset(indexMerged, parameter=c("PSAL", "DOWN_IRRADIANCE380"))
#'
#' # Example 6: subset data for the year 2019
#' data(index)
#' from <- as.POSIXct("2019-01-01", tz="UTC")
#' to <- as.POSIXct("2019-12-31", tz="UTC")
#' index6 <- subset(index, time=list(from=from, to=to))
#'
#' # Example 7: subset to the Canadian MEDS data
#' index7 <- subset(index, institution="ME")
#'
#' # Example 8: subset data to only include deep argo
#' \dontrun{
#' ai <- getIndex(file='merged', destdir = '~/data/argo')
#' index8 <- subset(ai, deep=TRUE) }
#'
#' # Example 9: subset to a specific ID
#' \dontrun{
#' ai <- getIndex(file='merged', destdir = '~/data/argo')
#' index9 <- subset(ai, ID='1900722') }
#'
#' @author Dan Kelley and Jaimie Harbin
#'
#' @importFrom oce geodDist
#' @importFrom sf st_polygon st_multipoint st_intersection
#' @export
setMethod(f="subset",
          signature="argoFloats",
          definition=function(x, subset=NULL, ...) {
              ##subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              dots <- list(...)
              dotsNames <- names(dots)
              if (missing(subset)) {
                  if (length(dots) == 0)
                      stop("must specify the subset, with 'subset' argument,'circle','rectangle', 'parameter','polygon', 'time', 'institution', 'deep', 'ID'")
                  if (length(dots) > 1)
                      stop("in subset,argoFloats-method() : cannot give more than one method in the '...' argument", call.=FALSE)
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
                      if (length(polygon) != 2)
                          stop("In subset,argoFloats-method() : 'polygon' must be a list of two elements")
                      if (2 != sum(c("longitude", "latitude") %in% names(polygon)))
                          stop("In subset,argoFloats-method() : 'polygon' must be a list containing 'longitude' and 'latitude'")
                      plat <- polygon$latitude
                      plon <- polygon$longitude
                      if (length(plat) != length(plon))
                          stop("lengths of polygon$longitude and polygon$latitude must match, but they are ",
                               length(plat), " and ", length(plon))
                      if ((head(plon, 1) != tail(plon, 1)) || head(plat, 1) != tail(plat, 1)) {
                          warning("In subset,argoFloats-method(): closing the polygon, since its first and last points did not match\n", call.=FALSE)
                          ##debug cat("before\n")
                          ##debug print(data.frame(plon, plat))
                          plon <- c(plon, plon[1])
                          plat <- c(plat, plat[1])
                          ##debug cat("after\n")
                          ##debug print(data.frame(plon, plat))
                      }
                      Polygon <- sf::st_polygon(list(outer=cbind(plon, plat)))
                      Points <- sf::st_multipoint(cbind(x[["longitude"]], x[["latitude"]]))
                      Inside <- sf::st_intersection(Points, Polygon)
                      M <- matrix(Points %in% Inside, ncol=2)
                      keep <- M[,1] & M[,2]
                      ##> keepOLD <- as.logical(sp::point.in.polygon(x[["longitude"]], x[["latitude"]],
                      ##>                                            polygon$longitude, polygon$latitude))
                      ##> if (sum(keep != keepOLD) != 0) {
                      ##>     message("problem with 'keep' and 'keepOLD'.  Developers should uncomment the 'browser' in next line and rebuilt, then retest")
                      ##>     # browser()
                      ##> }
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
                      if (length(institution) > 1)
                          stop("'institution' cannot hold more than one element")
                      keep <- grepl(institution, x@data$index$institution)
                      keep[is.na(keep)] <- FALSE
                      message("Kept ", sum(keep), " profiles (", sprintf("%.2g", 100*sum(keep)/length(keep)), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1]=='deep') {
                      deep <- dots[[1]]
                      if (!as.logical(deep))
                          stop("deep must be a logical vector indicating TRUE or FALSE")
                      if (deep) {
                          keep <- grep("849|862|864", x@data$index$profiler_type)
                      } else {
                          keep <- grep("849|862|864", x@data$index$profiler_type, invert=TRUE)
                      }
                      message("Kept ", length(keep), " profiles (", sprintf("%.2g", 100*length(keep)/length(keep)), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1] == 'ID') {
                      ID <- dots[[1]]
                      file <- x@data$index$file
                      fileID <- gsub("^[a-z]*/([0-9]*)/profiles/[A-Z]*[0-9]*_[0-9]{3}.nc$", "\\1", file)
                      keep <- ID == fileID
                      message("Kept ", sum(keep), " profiles (", sprintf("%.2g", 100*sum(keep)/length(keep)), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else {
                      stop("In subset,argoFloats-method() : the only permitted '...' argument is a list named 'circle','rectangle','parameter','polygon', 'time','institution', 'deep', or 'ID'", call.=FALSE)
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
          }
)


