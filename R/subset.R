## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

##OLD use_sf_package <- TRUE

#' Subset an argoFloats Object
#'
#' Isolate a subset of an [`argoFloats-class`] object of type `"index"`,
#' as created by [getIndex()], either by specifying
#' indices to keep (using the `subset` argument) or by specifying
#' a way to determine those indices (using the `...` argument).
#' Note that only one subset condition may be given in the `...`
#' argument, but that [merge,argoFloats-method()]
#' can be used to merge indices  created by `subset`,
#' which effectively creates a logical "or" operation.
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
#' will be retained. The polygon must not be self-intersecting,
#' and an error message will be issued if it is.  If the polygon is not closed
#' (i.e. if the first and last points do not coincide) the first point is pasted
#' onto the end, to close it.
#' See example 4.
#'
#' 5. A vector or list named `parameter` that holds character values that
#' specify the names of measured parameters to keep. See section 3.3 of the
#' Argo User's Manual, V3.3 (Carval et al. 2019) for a list of parameters.
#' See example 5.
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
#' See example 7.
#'
#' 8. A list named `deep` that holds a logical value indicating weather argo floats
#' are deep argo (i.e. `profiler_type` 849, 862, and 864). See example 8.
#'
#' 9. A list named `ID` that holds a character value specifying a float identifier.
#' See example 9.
#'
#' 10. A list named `ocean`, which holds a single character element that names the
#' ocean. The permitted values are:
#' `"A"` for Atlantic Ocean Area, from 70 W to 20 E,
#' `"P"` for Pacific Ocean Area, from 145 E to 70 W, and
#' `"I"` for Indian Ocean Area, from 20 E to 145 E.
#' See example 10.
#'
#' 11. A character value named `mode`, equal to either `realtime` or `delayed`,
#' that selects whether to retain real-time data or delayed data.
#' See example 11.
#'
#' 12. An integer value named `profile` that specifies which profiles are to be retained.
#' See example 12.
#'
#' In all cases, the notation is that longitude is positive
#' for degrees East and negative for degrees West, and that latitude
#' is positive for degrees North and negative for degrees South.
#'
#' @param x an [`argoFloats-class`] object as created by [getIndex()].
#'
#' @param subset optional numerical or logical vector that indicates which
#' indices of `x@data$index` to keep (example 1).
#'
#' @param ... the first entry here must be either (a)
#' a list named `circle`, `rectangle`, `polygon`,
#' `parameter`, `time`, `institution`, `id`,`ocean`,`mode`, or `profile`.
#' (examples 2 through 8)
#' or (b) a logical value named `deep` (example 9).  Optionally, this entry
#' may be followed by second entry named `silent`, which is a logical
#' value indicating whether to prevent the printing of messages that
#' indicate the number (and percentage) of data that are kept
#' during the subsetting operation.
#' See \dQuote{Details} and \dQuote{Examples}.
#'
#' @return An [`argoFloats-class`] object.
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
#' poly <- list(longitude=c(-77.492, -78.219, -77.904, -77.213, -76.728, -77.492),
#'              latitude=c(26.244, 25.247, 24.749, 24.987, 25.421, 26.244))
#' index4 <- subset(index, polygon=poly)
#'
#' # Show some of these subsets on a map
#' plot(index, bathymetry=FALSE)
#' points(index2[["longitude"]], index2[["latitude"]], col=2, pch=20, cex=1.4)
#' points(index3[["longitude"]], index3[["latitude"]], col=3, pch=20, cex=1.4)
#' rect(lonRect[1], latRect[1], lonRect[2], latRect[2], border=3, lwd=2)
#' points(index4[["longitude"]], index4[["latitude"]], col=4, pch=20, cex=1.4)
#' polygon(poly$longitude, poly$latitude, border=4)
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
#' # Example 8: subset to a specific ID
#' \dontrun{
#' ai <- getIndex(filename='merged', destdir = '~/data/argo')
#' index9 <- subset(ai, ID='1900722') }
#'
#' # Example 9: subset data to only include deep argo
#' \dontrun{
#' ai <- getIndex(filename=='merged', destdir = '~/data/argo')
#' index8 <- subset(ai, deep=TRUE) }
#'
#' # Example 10: subset data by ocean
#' \dontrun{
#' ai <- getIndex()
#' index10 <- subset(ai, circle=list(longitude=-83, latitude=9, radius=500))
#' plot(index10, which='map') # To get a visual
#' atlantic <- subset(index10, ocean='A') # Subsetting for Atlantic Ocean
#' pacific <- subset(index10, ocean='P')
#' points(atlantic[['longitude']], atlantic[['latitude']], pch=20, col=2)
#' points(pacific[['longitude']], pacific[['latitude']], pch=20, col=3) }
#'
#' # Example 11: subset by delayed time
#' \dontrun{
#' data(indexBgc)
#' index11 <- subset(index, mode='delayed')
#' profiles <- getProfiles(index11)
#' argos <- readProfiles(profiles)
#' oxygen <- unlist(argos[['oxygen']])
#' pressure <- unlist(argos[['pressure']])
#' plot(oxygen, pressure, ylim=rev(range(pressure, na.rm=TRUE)),
#' ylab='Pressure (dbar)', xlab="Oxygen (umol/kg)")
#' }
#'
#' # Example 12: subset by profile
#' \dontrun{
#' data(index)
#' index12 <- subset(index, profile='124')
#' cat("File names with profile number 124:", paste(index12[["file"]]), "\n")
#' }
#'
#' @author Dan Kelley and Jaimie Harbin
#'
## @importFrom oce geodDist
## @importFrom sp point.in.polygon
## @importFrom sf st_is_valid st_polygon st_multipoint st_intersection
#' @export
setMethod(f="subset",
          signature="argoFloats",
          definition=function(x, subset=NULL, ...) {
              ##subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              dots <- list(...)
              dotsNames <- names(dots)
              silent <- "silent" %in% dotsNames && dots$silent
              if (missing(subset)) {
                  if (length(dots) == 0)
                      stop("must specify the subset, with 'subset' argument,'circle','rectangle', 'parameter','polygon', 'time', 'institution', 'deep', 'ID', 'ocean', 'mode', or 'profile'")
                  if (length(dots) > 1) {
                      if (length(dots) > 2 || !("silent" %in% dotsNames))
                          stop("in subset,argoFloats-method() : cannot give more than one method in the '...' argument", call.=FALSE)
                  }
                  N <- length(x@data$index[[1]]) # used in calculating percentages
                  if (dotsNames[1] == "circle") {
                      circle <- dots[[1]]
                      if (!is.list(dots[1]))
                          stop("In subset,argoFloats-method() : 'circle' must be a list containing 'longitude', 'latitude' and 'radius'.")
                      if (3 != sum(c("longitude", "latitude", "radius") %in% sort(names(circle))))
                          stop("In subset,argoFloats-method() : 'circle' must be a list containing 'longitude', 'latitude' and 'radius'")
                      if (!requireNamespace("oce", quietly=TRUE))
                          stop("must install.packages(\"oce\") to subset by circle")
                      dist <- oce::geodDist(x[["longitude"]], x[["latitude"]], circle$longitude, circle$latitude)
                      keep <- dist < circle$radius
                      keep[is.na(keep)] <- FALSE
                      x@data$index <- x@data$index[keep, ]
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
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
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                  } else if (dotsNames[1]=="parameter") {
                      parameter <- dots[[1]]
                      if (is.list(dots[1]))
                          parameters <- unlist(parameter)
                      nparameters <- length(parameters)
                      parametersList <- lapply(x[["parameters"]], function(p) strsplit(p, " ")[[1]])
                      keep <- unlist(lapply(parametersList, function(pl) nparameters == sum(parameters %in% pl)))
                      if (sum(keep) < 1)
                          warning("In subset,argoFloats-method(..., parameter) : found no profiles with given parameter", call.=FALSE)
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1]=="polygon") {
                      if (!requireNamespace("sf", quietly=TRUE))
                          stop("must install.packages(\"sf\") for subset() by polygon to work")

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
                          #warning("In subset,argoFloats-method(): Closing the polygon, since the first and last points did not match.\n", call.=FALSE)
                          plon <- c(plon, plon[1])
                          plat <- c(plat, plat[1])
                      }
                      alon <- x[["longitude"]]
                      alat <- x[["latitude"]]
                      ##OLD if (use_sf_package) {
                      ## We need the *index* of points to keep, and not just a lon-lat subset of
                      ## points.  It is not too difficult to get the index with the 'sp'
                      ## package, but the only solution I could come up with using the 'sf'
                      ## package is to tack 1,2,3,etc onto the lon-lat points as a third
                      ## dimension, so that after we select for the points inside, we can skim
                      ## that third dimension and that gives us the 'keep' that we need. There
                      ## may be a more straightforward way, but my (admittedly shallow) reading
                      ## of the 'sf' function list did not uncover anything promising, and my
                      ## tests show that this scheme works.
                      ##
                      ## See https://github.com/ArgoCanada/argoFloats/issues/86
                      ok <- is.finite(alon) & is.finite(alat)
                      if (!requireNamespace("sf", quietly=TRUE))
                          stop("must install sf package for subset(...,polygon,...) to this to work")
                      Polygon <- sf::st_polygon(list(outer=cbind(plon, plat, rep(0, length(plon)))))
                      ## DOES NOT WORK (REQUIRES OTHER SOFTWARE??): Polygon <- sf::st_make_valid(Polygon)
                      if (!is.finite(sf::st_is_valid(Polygon))) {
                          errorMessage <- sf::st_is_valid(Polygon, reason=TRUE)
                          stop(paste0("In subset,argoFloats-method(): polygon is invalid, because of ", errorMessage), call.=FALSE)
                      }
                      ## multipoint does not permit NA values, so we set them to zero and remove them later
                      Points <- sf::st_multipoint(cbind(ifelse(ok, alon, 0),
                                                        ifelse(ok, alat, 0),
                                                        seq_along(alon)))
                      if (!sf::st_is_valid(Points)) {
                          errorMessage <- sf::st_is_valid(Points, reason=TRUE)
                          stop(paste0("In subset,argoFloats-method(): 'Points' is invalid, because of ", errorMessage), call.=FALSE)
                      }
                      Intersection <- sf::st_intersection(Points, Polygon)
                      keep <- Intersection[,3]
                      if (!silent)
                          message("Kept ", length(keep), " profiles (", sprintf("%.3g", 100*length(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                      ##OLD } else {
                      ##OLD     keep <- 0 != sp::point.in.polygon(alon, alat, plon, plat)
                      ##OLD     if (!silent)
                      ##OLD         message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/N), "%) using sp method")
                      ##OLD     x@data$index <- x@data$index[keep, ]
                      ##OLD }
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
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if(dotsNames[1]=="institution") {
                      institution <- dots[[1]]
                      if(!is.list(dots[1]))
                          stop("In subset,argoFloats-method() : 'institution' must be a list")
                      if (length(institution) > 1)
                          stop("'institution' cannot hold more than one element")
                      keep <- grepl(institution, x@data$index$institution)
                      keep[is.na(keep)] <- FALSE
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1] == 'deep') {
                      deep <- dots[[1]]
                      if (!as.logical(deep))
                          stop("deep must be a logical vector indicating TRUE or FALSE")
                      if (deep) {
                          keep <- grep("849|862|864", x@data$index$profiler_type)
                      } else {
                          keep <- grep("849|862|864", x@data$index$profiler_type, invert=TRUE)
                      }
                      if (!silent)
                          message("Kept ", length(keep), " profiles (", sprintf("%.3g", 100*length(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1] == 'ID') {
                      ID <- dots[[1]]
                      file <- x@data$index$file
                      fileID <- gsub("^[a-z]*/([0-9]*)/profiles/[A-Z]*[0-9]*_[0-9]{3}.nc$", "\\1", file)
                      keep <- ID == fileID
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if(dotsNames[1]=="ocean") {
                      ocean <- dots[[1]]
                      if(!is.list(dots[1]))
                          stop("In subset,argoFloats-method() : 'ocean' must be a list")
                      if (length(ocean) > 1)
                          stop("'ocean' cannot hold more than one element")
                      keep <- grepl(ocean, x@data$index$ocean)
                      keep[is.na(keep)] <- FALSE
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100.0*sum(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1]=="mode") {
                      mode <- dots[[1]]
                      if (!is.character(mode))
                          stop("In subset,argoFloats-method() : 'mode' must be character value")
                      if (mode == 'delayed') {
                          keep <- grepl("^[a-z]*/[0-9]*/profiles/.{0,1}D.*$", x[["file"]])
                      } else if (mode == 'realtime') {
                          keep <- grepl("^[a-z]*/[0-9]*/profiles/.{0,1}R.*$", x[["file"]])
                      } else {
                          stop("In subset,argoFloats-method() : 'mode' must be either 'realtime' or 'delayed', not '", mode, "'")
                      }
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100.0*sum(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1]=="profile") {
                      profile <- dots[[1]]
                      if (is.list(dots[1]))
                          profile <- unlist(profile)
                      nprofile <- length(profile)
                      profileList <- lapply(x[["profile"]], function(p) strsplit(p, " ")[[1]])
                      keep <- unlist(lapply(profileList, function(pl) nprofile == sum(profile %in% pl)))
                      if (sum(keep) < 1)
                          warning("In subset,argoFloats-method(..., parameter) : found no profiles with given profile", call.=FALSE)
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else {
                      stop("In subset,argoFloats-method() : the only permitted '...' argument is a list named 'circle','rectangle','parameter','polygon', 'time','institution', 'deep', 'ID', 'ocean', 'mode', or 'profile'", call.=FALSE)
                  }
              } else {
                  if (length(dotsNames) != 0)
                      stop("in subset,argoFloats-method() : cannot give both 'subset' and '...' arguments", call.=FALSE)
                  if (x@metadata$type == "index") {
                      if (!silent) {
                          if (is.logical(subset)) # this simplifies the percentage count for the method
                              subset <- which(subset)
                          message("Kept ", length(subset), " profiles (", sprintf("%.3g", 100.0*length(subset)/dim(x@data$index)[1]), "%)")
                      }
                      x@data$index <- x@data$index[subset, ]
                  } else {
                      stop("In subset,argoFloats-method() : method not coded except for type=\"index\"", call.=FALSE)
                  }
              }
              x
          }
)


