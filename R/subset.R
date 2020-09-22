## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

##OLD use_sf_package <- TRUE

#' Subset an argoFloats Object
#'
#' Return a subset of an [`argoFloats-class`] for two object types:
#' A) type `"index"`, as created by [getIndex()], either by specifying indices
#' to keep (using the `subset` argument) or by specifying a way to determine
#' those indices (using the `...` argument). Note that only one subset condition
#' may be given in the `...` argument, but that [merge,argoFloats-method()] can
#' be used to merge indices  created by `subset`, which effectively creates a
#' logical "or" operation.
#' B) type `"argos"`, as created by [readProfiles()]. Note that the only subset
#' condition that can be give in the `...` argument is `column` or `cycle` for `argos`
#' type.
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
#' 9. A list named `id` that holds a character value specifying a float identifier.
#' See example 9.
#'
#' 10. A list named `ocean`, which holds a single character element that names the
#' ocean. The permitted values are:
#' `"A"` for Atlantic Ocean Area, from 70 W to 20 E,
#' `"P"` for Pacific Ocean Area, from 145 E to 70 W, and
#' `"I"` for Indian Ocean Area, from 20 E to 145 E.
#' See example 10.
#'
#' 11. A character value named `dataMode`, equal to either `realtime` or `delayed`,
#' that selects whether to retain real-time data or delayed data.
#' See example 11.
#'
#' 12. An integer value named `cycle` that specifies which cycles are to be retained.
#' This is done by regular-expression matching of the filename, looking between the
#' underline character (`"_"`) and the suffix (`.nc`), but note that the expression
#' is made up of a compulsory component comprising 3 or 4 digits, and an optional
#' component that is either blank or the character `"D"` (which designates a
#' descending profile).  Thus, `001` will match both `*_001.nc` and `*_001D.nc`.
#' Note this can be used for both `"index"` and `"argos"` types.
#' See example 12.
#'
#' 13. A character value named `direction`, equal to either `decent` or `ascent`,
#' that selects whether to retain data from the ascent or decent phase.
#' See example 13.
#'
#' 14. An integer value named `column`, that selects which column of parameters
#' to obtain. Note that this type of subset is possible for `argos` and `index`
#' type objects.
#'
#' 15. A integer value named `debug` that controls whether `subset()` prints
#' some information to describe what it is doing.
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
#' @param ... the first entry here must be either (a) a list named `circle`,
#' `rectangle`, `polygon`, `parameter`, `time`, `institution`,
#' `id`,`ocean`,`dataMode`,`cycle`, `direction`, or `column`.
#'  (examples 2 through 8, and 10 through 14),
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
#' # Example 5: subset synthetic data containing 'DOXY' parameters
#' # Data containing 'DOXY' data
#' data(indexSynthetic)
#' index5A <- subset(indexSynthetic, parameter="DOXY")
#' # Data containing both 'PSAL' and 'DOWN_IRRADIANCE380' data
#' data(indexSynthetic)
#' index5B <- subset(indexSynthetic, parameter=c("PSAL", "DOWN_IRRADIANCE380"))
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
#' # Example 8: subset to a specific id
#' \dontrun{
#' ai <- getIndex(filename='synthetic', destdir = '~/data/argo')
#' index9 <- subset(ai, id='1900722') }
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
#' index11 <- subset(index, dataMode='delayed')
#' profiles <- getProfiles(index11)
#' argos <- readProfiles(profiles)
#' oxygen <- unlist(argos[['oxygen']])
#' pressure <- unlist(argos[['pressure']])
#' plot(oxygen, pressure, ylim=rev(range(pressure, na.rm=TRUE)),
#' ylab='Pressure (dbar)', xlab="Oxygen (umol/kg)")
#' }
#'
#' # Example 12: subset by cycle
#' \dontrun{
#' data(index)
#' index12 <- subset(index, cycle='124')
#' cat("File names with cycle number 124:", paste(index12[["file"]]), "\n")
#' }
#'
#' # Example 13: subset by direction
#' \dontrun{
#' library(argoFloats)
#' index13A <- subset(getIndex(), deep=TRUE)
#' index13B <- subset(deep, direction='decent')
#' index13B[['file']]
#' }
#'
#' # Example 14: subset by column (for argos type)
#' \dontrun{
#' library(argoFloats)
#' index14A <- subset(getIndex(filename='merge'), id="5903889")
#' index14B <- subset(index14A, cycle='074')
#' argos14A <- readProfiles(getProfiles(index14B))
#' argos14B <- subset(argos14A, column=1)
#' argos14C <- subset(argos14A, column=2)
#' D <- data.frame(Oxygen = argos14A[['oxygen']],
#' col1= argos14B[['oxygen']][[1]],
#' col2=argos14C[['oxygen']][[1]])
#' }
#'
#' # Example 15: subset by cycle (for argos type) to create TS diagram
#' \dontrun{
#' data("index")
#' index15 <- subset(index, id="1901584")
#' profiles <- getProfiles(index15)
#' argos <- readProfiles(profiles)
#' plot(subset(argos, cycle='147'), which='TS')
#' }
#'
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
              ## subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              dots <- list(...)
              dotsNames <- names(dots)
              ## Clear the 'debug' and 'silent' entries from dots, because later we insist that length(dots) be 1.
              debug <- 0
              if ("debug" %in% dotsNames) {
                  debug <- dots$debug
                  dots$debug <- NULL
                  dotsNames <- names(dots)
              }
              silent <- 0
              if ("silent" %in% dotsNames) {
                  silent <- dots$silent
                  dots$silent <- NULL
                  dotsNames <- names(dots)
              }
              ## All done with manipulating dots now.

              argoFloatsDebug(debug, "subset,argoFloats-method() {\n", style="bold", sep="", unindent=1)
              ## message("type =", x@metadata$type)
              if (x@metadata$type == "profiles") {
                  stop("in subset,argoFloats-method() :\n  subset doesn't work for type = profiles", call.=FALSE)
              }
              ## Step 1: handle 'argo' type first. Note that 'subset' is ignored; rather, we insist that either
              ## 'column' or 'cycle' be provided.
              if (x@metadata$type == "argos") {
                  argoFloatsDebug(debug, "subsetting with type=\"argos\"\n")
                  if (length(dotsNames) == 0)
                      stop("in subset,argoFloats-method() :\n  must give 'column' or 'cycle' argument", call.=FALSE)
                  if (dotsNames[1] == "column") {
                      argoFloatsDebug(debug, "subsetting by column ", column, "\n")
                      column <- dots[[1]]
                      ## Loop over all objects within the data, and within that loop look at data within the object,
                      ## and for each of them, if its a vactor subset according to column and if its a matrix
                      ## subset according to column
                      res <- x
                      argos <- x[['argos']]
                      ## Loop over all objects
                      ##message("column=",paste(column, collapse=" "))
                      for (iargo in seq_along(argos)) {
                          argo <- argos[[iargo]]
                          ## Handle the metadata slot
                          for (name in names(argo@metadata)) {
                              ##message("name=",name)
                              argoFloatsDebug(debug, "subsetting metadata item named '", name, "'\n", sep="")
                              ## Pass some things through directly.
                              if (name %in% c("units", "filename", "flagScheme", "dataNamesOriginal"))
                                  next
                              item <- argo@metadata[[name]]
                              ## Handle things that are encoded as characters in a string,
                              ## namely 'direction', 'juldQC', and 'positionQC'.
                              if (name == "direction" || grepl("QC$", name)) {
                                 ## message("  -- character")
                                  res@data$argos[[iargo]]@metadata[[name]] <- paste(strsplit(item,"")[[1]][column],collapse="")
                              } else if (is.list(item)) {
                                  ##message("list")
                                  for (l in seq_along(item)) {
                                      ##print(dim(item[[l]]))
                                      D <- dim(item[[l]])
                                      if (column > D[2])
                                          stop("cannot access column ", column, " of metadata item \"", name, "\" because its dimension is ", paste(D, collapse=" "))
                                      ##cat("BEFORE:\n");print(dim(res@data$argos[[iargo]]@metadata[[name]][[l]]))
                                      res@data$argos[[iargo]]@metadata[[name]][[l]] <- item[[l]][, column, drop=FALSE]
                                      ##cat("AFTER:\n");print(dim(res@data$argos[[iargo]]@metadata[[name]][[l]]))
                                  }
                              } else if (is.vector(name)) {
                                  ##message("vector")
                                  res@data$argos[[iargo]]@metadata[[name]] <- item[column]
                              } else if (is.matrix(name)) {
                                  ##message("matrix")
                                  res@data$argos[[iargo]]@metadata[[name]] <- item[, column, drop=FALSE]
                              } else if (is.array(name)) {
                                  argoFloatsDebug(debug, "name=", name, " has dim ", paste(dim(res@metadata[[name]]), collapse=" "), "\n")
                                  if (length(dim(res@metadata[[name]])) <= 3) {
                                      res@metadata[[name]] <- item[, , keep, drop=FALSE]
                                  } else {
                                      warning("not subsetting \"", name, "\" in metadata, because it is an array of rank > 3")
                                  }
                              } else {
                                  stop("cannot subset metadata item named '", name, "' because it is not a length-one string, a vector, or a matrix")
                              }
                          }
                          ## Handle the data slot
                          for (name in names(argo@data)) {
                              item <- argo@data[[name]]
                              if (is.matrix(item)) {
                                  dim <- dim(item)
                                  if (column > dim[2])
                                      stop("in subset,argoFloats-method() :\n  Only have ", dim[2], " columns", call.=FALSE)
                                  newItem <- item[, column, drop=FALSE]
                                  res@data$argos[[iargo]]@data[[name]] <- newItem
                              } else {
                                  length <- length(item)
                                  if (column > length)
                                      stop("in subset,argoFloats-method() :\n  Only have ", length, " columns", call.=FALSE)
                                  newItem <- item[column, drop=FALSE]
                                  res@data$argos[[iargo]]@data[[name]] <- newItem
                              }
                          }
                      }
                      argoFloatsDebug(debug, "} # subset,argoFloats-method()\n", style="bold", sep="", unindent=1)
                      return(res)
                  } else if (dotsNames[1] == 'cycle') {
                      argoFloatsDebug(debug, "subsetting by cycle for 'argos' type\n")
                      cycle <- dots[[1]]
                      file <- unlist(x[['filename']])
                      fileCycle <- sapply(x[['data']][[1]], function(x) x[["cycleNumber"]])
                      ## Insist that the cycles are available
                      keep <- rep(FALSE, length(fileCycle))
                      for (thisCycle in cycle) {
                          if (!(thisCycle %in% fileCycle))
                              stop("In subset,argoFloats-method(): Cycle '", thisCycle, "' not found. Try one of: ", paste(fileCycle, collapse=', '), call.=FALSE)
                          keep <- keep | (thisCycle == fileCycle)
                      }
                      res <- x
                      res@data[[1]] <- x@data[[1]][keep]
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/length(keep)), "%)")
                      argoFloatsDebug(debug, "} # subset,argoFloats-method()\n", style="bold", sep="", unindent=1)
                      return(res)
                  } else {
                      stop("in subset,argoFloats-method():\n  the only permitted '...' argument for argos type is 'column' or 'cycle'", call.=FALSE)
                  }
              }
              ## Step 2: Now we know the type is either 'index' or 'profiles'.  In either case,
              ## 'subset' can be provided, so we check for its existence first.
              if (missing(subset)) {
                  #argoFloatsDebug(debug, "no subset was given, so it must be circle=, rectangle=, or similar\n")
                  if (length(dots) == 0)
                      stop("in subset,argoFloats-method() :\n for indices, must specify the subset, with 'subset' argument, 'circle','rectangle', 'parameter','polygon', 'time', 'institution', 'deep', 'id', 'ocean', dataMode', 'cycle', or 'direction'")
                  if (length(dots) > 2)
                      stop("in subset,argoFloats-method() :\n  cannot give more than one method in the '...' argument", call.=FALSE)
                  N <- length(x@data$index[[1]]) # used in calculating percentages
                  if (x@metadata$type == "index") {
                      argoFloatsDebug(debug, "subsetting with type=\"index\"\n")
                  if (dotsNames[1] == "circle") {
                      argoFloatsDebug(debug, "subsetting by circle\n")
                      circle <- dots[[1]]
                      if (!is.list(dots[1]))
                          stop("in subset,argoFloats-method() :\n  'circle' must be a list containing 'longitude', 'latitude' and 'radius'.")
                      if (3 != sum(c("longitude", "latitude", "radius") %in% sort(names(circle))))
                          stop("in subset,argoFloats-method() :\n  'circle' must be a list containing 'longitude', 'latitude' and 'radius'")
                      if (!requireNamespace("oce", quietly=TRUE))
                          stop("must install.packages(\"oce\") to subset by circle")
                      dist <- oce::geodDist(x[["longitude"]], x[["latitude"]], circle$longitude, circle$latitude)
                      keep <- dist < circle$radius
                      keep[is.na(keep)] <- FALSE
                      x@data$index <- x@data$index[keep, ]
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                  } else if (dotsNames[1] == "rectangle") {
                      argoFloatsDebug(debug, "subsetting by rectangle\n")
                      rectangle <- dots[[1]]
                      if (!is.list(dots[1]))
                          stop("in subset,argoFloats-method():\n  'rectangle' must be a list containing 'longitude' and 'latitude'")
                      if (2 != sum(c("longitude", "latitude") %in% sort(names(rectangle))))
                          stop("in subset,argoFloats-method():\n  'rectangle' must be a list containing 'longitude' and 'latitude'")
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
                      argoFloatsDebug(debug, "subsetting by parameter\n")
                      parameter <- dots[[1]]
                      if (is.list(dots[1]))
                          parameters <- unlist(parameter)
                      nparameters <- length(parameters)
                      parametersList <- lapply(x[["parameters"]], function(p) strsplit(p, " ")[[1]])
                      keep <- unlist(lapply(parametersList, function(pl) nparameters == sum(parameters %in% pl)))
                      if (sum(keep) < 1)
                          warning("in subset,argoFloats-method(..., parameter):\n  found no profiles with given parameter", call.=FALSE)
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1]=="polygon") {
                      argoFloatsDebug(debug, "subsetting by polygon\n")
                      if (!requireNamespace("sf", quietly=TRUE))
                          stop("must install.packages(\"sf\") for subset() by polygon to work")
                      polygon <- dots[[1]]
                      if(!is.list(dots[1]))
                          stop("in subset,argoFloats-method():\n  'polygon' must be a list")
                      if (length(polygon) != 2)
                          stop("in subset,argoFloats-method():\n  'polygon' must be a list of two elements")
                      if (2 != sum(c("longitude", "latitude") %in% names(polygon)))
                          stop("in subset,argoFloats-method():\n  'polygon' must be a list containing 'longitude' and 'latitude'")
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
                          stop(paste0("Error in subset,argoFloats-method():\n  polygon is invalid, because of ", errorMessage), call.=FALSE)
                      }
                      ## multipoint does not permit NA values, so we set them to zero and remove them later
                      Points <- sf::st_multipoint(cbind(ifelse(ok, alon, 0),
                                                        ifelse(ok, alat, 0),
                                                        seq_along(alon)))
                      if (!sf::st_is_valid(Points)) {
                          errorMessage <- sf::st_is_valid(Points, reason=TRUE)
                          stop(paste0("Error in subset,argoFloats-method():\n  'Points' is invalid, because of ", errorMessage), call.=FALSE)
                      }
                      Intersection <- sf::st_intersection(Points, Polygon)
                      keep <- Intersection[,3]
                      if (!silent)
                          message("Kept ", length(keep), " profiles (", sprintf("%.3g", 100*length(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1]=="time") {
                      argoFloatsDebug(debug, "subsetting by time\n")
                      time <- dots[[1]]
                      if(!is.list(dots[1]))
                          stop("in subset,argoFloats-method():\n  'time' must be a list")
                      if (!inherits(time$from, "POSIXt"))
                          stop("'time' must be a list containing POSIX times")
                      if (2 != sum(c("from", "to") %in% names(time)))
                          stop("in subset,argoFloats-method():\n  'time' must be a list containing 'to'and 'from'")
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
                      argoFloatsDebug(debug, "subsetting by institution\n")
                      institution <- dots[[1]]
                      if(!is.list(dots[1]))
                          stop("in subset,argoFloats-method():\n  'institution' must be a list")
                      if (length(institution) > 1)
                          stop("'institution' cannot hold more than one element")
                      keep <- grepl(institution, x@data$index$institution)
                      keep[is.na(keep)] <- FALSE
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1] == 'deep') {
                      argoFloatsDebug(debug, "subsetting by deep\n")
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
                  } else if (dotsNames[1] == 'id' || dotsNames[1] == "ID") {
                      argoFloatsDebug(debug, "subsetting by id\n")
                      if (dotsNames[1] == "ID")
                          warning("In subset,argoFloats-method : converted subset(x,ID=...) to subset(x,id=...) for backwards compatibility\n  NOTE: this conversion will cease after 2020-Sep-01.", call.=FALSE)
                      id <- dots[[1]]
                      file <- x@data$index$file
                      fileid <- gsub("^[a-z]*/([0-9]*)/profiles/[A-Z]*[0-9]*_[0-9]{3}[A-Z]*.nc$", "\\1", file)
                      keep <- id == fileid
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1]=="ocean") {
                      argoFloatsDebug(debug, "subsetting by ocean\n")
                      ocean <- dots[[1]]
                      if(!is.list(dots[1]))
                          stop("in subset,argoFloats-method():\n  'ocean' must be a list")
                      if (length(ocean) > 1)
                          stop("'ocean' cannot hold more than one element")
                      keep <- grepl(ocean, x@data$index$ocean)
                      keep[is.na(keep)] <- FALSE
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100.0*sum(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1]=="dataMode") {
                      argoFloatsDebug(debug, "subsetting by dataMode\n")
                      dataMode <- dots[[1]]
                      if (!is.character(dataMode))
                          stop("in subset,argoFloats-method():\n  'dataMode' must be character value")
                      if (dataMode == 'delayed') {
                          keep <- grepl("^[a-z]*/[0-9]*/profiles/.{0,1}D.*$", x[["file"]])
                      } else if (dataMode == 'realtime') {
                          keep <- grepl("^[a-z]*/[0-9]*/profiles/.{0,1}R.*$", x[["file"]])
                      } else {
                          stop("in subset,argoFloats-method():\n  'dataMode' must be either 'realtime' or 'delayed', not '", dataMode, "'")
                      }
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100.0*sum(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1] == "profile" || dotsNames[1] == "cycle") {
                      argoFloatsDebug(debug, "subsetting by profile\n")
                      if (dotsNames[1] == "profile")
                          warning("In subset,argoFloats-method : converted subset(x,profile=...) to subset(x,cycle=...) for backwards compatibility\n  NOTE: this conversion will cease after 2020-Sep-01.", call.=FALSE)
                      cycle <- dots[[1]]
                      if (is.list(dots[1]))
                          cycle <- unlist(cycle)
                      cycle <- as.character(cycle)
                      ncycle <- length(cycle)
                      xcycle <- x[["cycle"]]
                      ##OLD cycleList <- lapply(x[["cycle"]], function(p) strsplit(p, " ")[[1]])
                      ##OLD keep <- unlist(lapply(cycleList, function(pl) ncycle == sum(cycle %in% pl)))
                      keep <- grepl(cycle[1], xcycle)
                      for (i in seq_along(cycle)) {
                          if (i > 1)
                              keep <- keep | grepl(cycle[i], xcycle)
                      }
                      if (sum(keep) < 1)
                          warning("In subset,argoFloats-method(..., parameter) : found no profiles with given profile", call.=FALSE)
                      if (!silent)
                          message("Kept ", sum(keep), " profiles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                      x@data$index <- x@data$index[keep, ]
                  } else if (dotsNames[1]=="direction") {
                      argoFloatsDebug(debug, "subsetting by direction\n")
                      direction <- dots[[1]]
                      if (!is.character(direction))
                          stop("in subset,argoFloats-method():\n  'direction' must be character value of either 'ascent' or 'decent'")
                      if (direction == 'ascent') {
                          keep <- grepl("^.*[^D].nc$", x@data$index$file)
                      } else if (direction == 'decent') {
                          keep <- grepl("^.*D.nc$", x@data$index$file)
                      } else {
                          stop("in subset,argoFloats-method():\n  'direction' must be either 'ascent' or 'decent', not '", direction, "'", call.=FALSE)
                      }
                      x@data$index <- x@data$index[keep, ]
                  } else {
                      stop("in subset,argoFloats-method():\n  the only permitted '...' argument for indices is a list named 'circle','rectangle','parameter','polygon', 'time','institution', 'deep', 'id', 'ocean', 'dataMode', 'cycle', or 'direction'", call.=FALSE)
                  }
                  }
              } else {
                  if (length(dotsNames) != 0)
                      stop("in subset,argoFloats-method():\n  cannot give both 'subset' and '...' arguments", call.=FALSE)
                  if (x@metadata$type == "index") {
                      if (!silent) {
                          if (is.logical(subset)) # this simplifies the percentage count for the method
                              subset <- which(subset)
                          message("Kept ", length(subset), " profiles (", sprintf("%.3g", 100.0*length(subset)/dim(x@data$index)[1]), "%)")
                      }
                      x@data$index <- x@data$index[subset, ]
                  } else {
                      stop("in subset,argoFloats-method():\n  method not coded except for type=\"index\"", call.=FALSE)
                  }
              }
              x
          }
          )


