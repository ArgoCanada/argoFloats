## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

##OLD use_sf_package <- TRUE

#' Subset an argoFloats Object
#'
#' Return a subset of an [`argoFloats-class`] object.  This applies
#' only to objects of type `"index"`, as created with [getIndex()] or `subset()`
#' acting on such a value, or of type `"argos"`, as created with [readProfiles()].
#' (It cannot be used on objects of type `"profiles"`, as created with [getProfiles()].)
#' There are two ways to use `subset()`.  **Method 1.** supply
#' the `subset` argument.  This may be a logical vector indicating
#' which entries to keep (by analogy to the base-R `subset()`
#' function) or it may be an integer vector holding the indices of entries to
#' be retained. **Method 2.** do not supply `subset`.  In this
#' case, the action is determined by the third (`...`)
#' argument; see \sQuote{Details}.
#'
#' The possibilities for the `...` argument are as follows.
#'
#' 1. An integer vector giving indices to keep.
#'
#' 2. A list named `circle` with numeric elements named `longitude`,
#' `latitude` and `radius`.  The first two give the center of
#' the subset region, and the third gives the radius of
#' that region, in kilometers.
#'
#' 3. A list named `rectangle` that has elements named
#' `longitude` and `latitude`, two-element numeric vectors
#' giving the western and eastern, and southern and northern
#' limits of the selection region.
#'
#' 4. A list named `polygon` that has elements named `longitude` and `latitude`
#' that are numeric vectors specifying a polygon within which profiles
#' will be retained. The polygon must not be self-intersecting,
#' and an error message will be issued if it is.  If the polygon is not closed
#' (i.e. if the first and last points do not coincide) the first point is pasted
#' onto the end, to close it.
#'
#' 5. A vector or list named `parameter` that holds character values that
#' specify the names of measured parameters to keep. See section 3.3 of
#' Reference 1 for a list of parameters.
#'
#' 6. A list named `time` that has elements `from` and `to` that are either
#' POSIXt times, or character strings that `subset()` will convert to
#' POSIXt times using [as.POSIXct()] with `tz="UTC"`.
#'
#' 7. A list named `institution` that holds a single character element that
#' names the institution.  The permitted values are:
#' `"AO"` for Atlantic Oceanographic and Meteorological Laboratory;
#' `"BO"` for British Oceanographic Data Centre;
#' `"CS"` for Commonwealth Scientific and Industrial Research Organization;
#' `"HZ"` for China Second Institute of Oceanography;
#' `"IF"` for Ifremer, France;
#' `"IN"` for India National Centre for Ocean Information Services;
#' `"JA"` for Japan Meteorological Agency;
#' `"KM"` for Korea Meteorological Agency;
#' `"KO"` for Korea Ocean Research and Development Institute;
#' `"ME"` for Marine Environment Data Section; and
#' `"NM"` for National Marine Data & Information Service.
#'
#' 8. A list named `deep` that holds a logical value indicating whether argo floats
#' are deep argo (i.e. `profiler_type` 849, 862, and 864).
#'
#' 9. A list named `ID` that holds a character value specifying a float identifier.
#'
#' 10. A list named `ocean` that  holds a single character element that names the
#' ocean. The permitted values are:
#' `"A"` for Atlantic Ocean Area, from 70 W to 20 E,
#' `"P"` for Pacific Ocean Area, from 145 E to 70 W, and
#' `"I"` for Indian Ocean Area, from 20 E to 145 E.
#'
#' 11. A character value named `dataMode`, equal to either `realtime` or `delayed`,
#' that selects whether to retain real-time data or delayed data.  There are two
#' possibilities, depending on the `type` of the `x` argument.
#' **Case 1.** If `x` is of `type="index"`, then the subset is done by looking for the letters
#' `R` or `D` in the source filename. Note that a file in the
#' latter category may contain some profiles that are of delayed mode *and also*
#' some profiles that are of `realtime` or `adjusted` mode.
#' **Case 2.** If `x` is
#' of type `argos`, then the subset operation is done for each profile within
#' the dataset. Sometimes this will yield data arrays with zero columns.
#'
#' 12. An integer or character value named `cycle` that specifies which cycles are to be retained.
#' This is done by regular-expression matching of the filename, looking between the
#' underline character (`"_"`) and the suffix (`.nc`), but note that the expression
#' is made up of a compulsory component comprising 3 or 4 digits, and an optional
#' component that is either blank or the character `"D"` (which designates a
#' descending profile).  Thus, `001` will match both `*_001.nc` and `*_001D.nc`.
#' Note this can be used for both `"index"` and `"argos"` types.
#'
#' 13. A character value named `direction`, equal to either "descent" or "ascent",
#' that selects whether to retain data from the ascent or decent phase.
#'
#' 14. An integer value named `profile` that selects which profiles
#' to retain.  Note that this type of subset is possible only
#' for objects of type `"argos"`.
#'
#' 15. An integer value named `cycle` that selects which cycles
#' to retain.
#'
#' 16. A character value named `dataStateIndicator`, equal to either "0A", "1A",
#' "2B", "2B+", "2C", "2C+", "3B", or "3C" that selects which `dataStateIndicator`
#' to keep (see table 6 of Reference 1 for details of these codes).
#' This operation only works for objects of type `"argos"`.
#'
#' 17. A list named `section` that  has four elements:
#' `longitude`,`latitude`, `width`, and `segments`. The first two of these
#' are numeric vectors that define the spine of the section, in degrees East
#' and North, respectively.  These are both mandatory, while both `width` and `segments`
#' are optional. If given, `width` is taken as maximal distance between an Argo
#' and the spine, for that float to be retained. If `width` is not given, it
#' defaults to 100km.  If given, `segments` is either `NULL` or a positive
#' integer.  Setting `segments` to `NULL` will cause the float-spine distance
#' to be computed along a great-circle route.  By contrast, if `segments` is an
#' integer, then the spine is traced using `stats::approx()`, creating
#' `segments` new points.  If `segments` is not provided, it defaults to 100.
#'
#' In all cases, the notation is that longitude is positive
#' for degrees East and negative for degrees West, and that latitude
#' is positive for degrees North and negative for degrees South.
#' For more on this function, see Kelley et al. (2021).
#'
#' @param x an [`argoFloats-class`] object as created by [getIndex()].
#'
#' @param subset optional numerical or logical vector that indicates which
#' indices of `x@data$index` to keep (example 1).
#'
#' @param ... the first entry here must be either (a) a list named `circle`,
#' `rectangle`, `polygon`, `parameter`, `time`, `institution`,
#' `ID`,`ocean`,`dataMode`,`cycle`, `direction`, `profile`, or `section`.
#'  (examples 2 through 8, and 10 through 17),
#' or (b) a logical value named `deep` (example 9).  Optionally, this entry
#' may be followed by second entry named `silent`, which is a logical
#' value indicating whether to prevent the printing of messages that
#' indicate the number (and percentage) of data that are kept
#' during the subsetting operation.
#' See \dQuote{Details} and \dQuote{Examples}.
#'
#' @return An [`argoFloats-class`] object, restricted as indicated.
#'
#' @examples
#' library(argoFloats)
#' data(index)
#' data(indexSynthetic)
#'
#' # Subset to the first 3 profiles in the (built-in) index
#' indexFirst3 <- subset(index, 1:3)
#'
#' # Subset to a circle near Abaco Island
#' indexCircle <- subset(index, circle=list(longitude=-77.5, latitude=27.5, radius=50))
##
## # NOTE: this example was removed because it requires using tempdir, which
## # is not something we want to encourage, since it goes against the whole idea
## # of caching.
## # Example 2B: subset a 300 km radius around Panama using "maps" package
## # This example requires downloading.
## \donttest{
## library("maps")
## data(world.cities)
## ai <- getIndex()
## panama <- subset(world.cities, name=="Panama")
## index1 <- subset(ai, circle=list(longitude=panama$long, latitude=panama$lat, radius=200))
##}
#'
#' # Subset to a rectangle near Abaco Island
#' lonlim <- c(-76.5, -76)
#' latlim <- c(26.5, 27.5)
#' indexRectangle <- subset(index, rectangle=list(longitude=lonlim, latitude=latlim))
#'
#' # Subset to a polygon near Abaco Island
#' lonp <- c(-77.492, -78.219, -77.904, -77.213, -76.728, -77.492)
#' latp <- c( 26.244,  25.247,  24.749,  24.987,  25.421,  26.244)
#' indexPolygon <- subset(index, polygon=list(longitude=lonp, latitude=latp))
##
## # Show some of these subsets on a map
## plot(index, bathymetry=FALSE)
## points(index2[["longitude"]], index2[["latitude"]], col=2, pch=20, cex=1.4)
## points(index3[["longitude"]], index3[["latitude"]], col=3, pch=20, cex=1.4)
## rect(lonRect[1], latRect[1], lonRect[2], latRect[2], border=3, lwd=2)
## points(index4[["longitude"]], index4[["latitude"]], col=4, pch=20, cex=1.4)
## polygon(p$longitude, p$latitude, border=4)
#'
#' # Subset to year 2019
#' index2019 <- subset(index, time=list(from="2019-01-01", to="2019-12-31"))
#'
#' # Subset to Canadian MEDS data
#' indexMEDS <- subset(index, institution="ME")
##
## # NOTE: this example was removed because it requires using tempdir, which
## # is not something we want to encourage, since it goes against the whole idea
## # of caching.
## # Example 8: subset to a specific ID
## # This example requires downloading.
## \donttest{
## ai <- getIndex(filename="synthetic")
## index9 <- subset(ai, ID="1900722")
##}
##
## # NOTE: this example was removed because it requires using tempdir, which
## # is not something we want to encourage, since it goes against the whole idea
## # of caching.
## # Example 9: subset data to only include deep argo
## # This example requires downloading.
## \donttest{
## ai <- getIndex(filename="synthetic")
## index8 <- subset(ai, deep=TRUE)
##}
##
## # NOTE: this example was removed because it requires using tempdir, which
## # is not something we want to encourage, since it goes against the whole idea
## # of caching.
## # Example 10: subset data by ocean
## # This example requires downloading.
## \donttest{
## ai <- getIndex()
## index10 <- subset(ai, circle=list(longitude=-83, latitude=9, radius=500))
## plot(index10, which="map", bathymetry=FALSE)
## atlantic <- subset(index10, ocean="A") # Subsetting for Atlantic Ocean
## pacific <- subset(index10, ocean="P")
## points(atlantic[["longitude"]], atlantic[["latitude"]], pch=20, col=2)
## points(pacific[["longitude"]], pacific[["latitude"]], pch=20, col=3)
##}
##
## # NOTE: there is no example 11.
##
## # NOTE: this example was removed because it requires using tempdir, which
## # is not something we want to encourage, since it goes against the whole idea
## # of caching.
## # Example 11: subset by delayed time
## # This example requires downloading.
## \donttest{
## data(indexBgc)
## index11 <- subset(indexBgc, dataMode="delayed")
## profiles <- getProfiles(index11)
## argos <- readProfiles(profiles)
## oxygen <- argos[["oxygen"]][[3]]
## pressure <- argos[["pressure"]][[3]]
## plot(oxygen, pressure, ylim=rev(range(pressure, na.rm=TRUE)),
##      ylab="Pressure (dbar)", xlab="Oxygen (umol/kg)")
##}
##
## # Example 12: subset by cycle
## data(index)
## index12A <- subset(index, cycle="124")
## index12B <- subset(index, cycle=0:2)
## cat("File names with cycle number 124:", paste(index12A[["file"]]), "\n")
## cat("File names with cycle number between 0 and 2:", paste(index12B[["file"]]), "\n")
##
##
## # NOTE: this example was removed because it requires using tempdir, which
## # is not something we want to encourage, since it goes against the whole idea
## # of caching.
## # Example 13: subset by direction
## # This example requires downloading.
## \donttest{
## library(argoFloats)
## index13A <- subset(getIndex(), deep=TRUE)
## index13B <- subset(index13A, direction="descent")
## head(index13B[["file"]])
##}
##
## # NOTE: this example was removed because it requires using tempdir, which
## # is not something we want to encourage, since it goes against the whole idea
## # of caching.
## # Example 14: subset by profile (for argos type)
## # This example requires downloading.
## \donttest{
## library(argoFloats)
## index14A <- subset(getIndex(filename="synthetic"), ID="5903889")
## index14B <- subset(index14A, cycle="074")
## argos14A <- readProfiles(getProfiles(index14B))
## argos14B <- subset(argos14A, profile=1)
## D <- data.frame(Oxygen = argos14A[["oxygen"]],
## col1= argos14B[["oxygen"]][[1]])
##}
##
## # NOTE: this example was removed because it requires using tempdir, which
## # is not something we want to encourage, since it goes against the whole idea
## # of caching.
## # Example 15: subset by cycle (for argos type) to create TS diagram
## # This example requires downloading.
## \donttest{
## data("index")
## index15 <- subset(index, ID="1901584")
## profiles <- getProfiles(index15)
## argos <- readProfiles(profiles)
## plot(subset(argos, cycle="147"), which="TS")
##}
##
## # NOTE: this example was removed because it requires using tempdir, which
## # is not something we want to encourage, since it goes against the whole idea
## # of caching.
## # Example 16: subset by dataStateIndicator
## # This example requires downloading.
## \donttest{
## data("index")
## index16 <- subset(index, 1:40)
## argos <- readProfiles(getProfiles(index16))
## argos16A <- subset(argos, dataStateIndicator="2C")
## argos16B <- subset(argos, dataStateIndicator="2B")
##}
##
## # Example 17: subset by section to create a map plot
## if (requireNamespace("s2")) {
##     data("index")
##     lon <- c(-78, -77, -76)
##     lat <-c(27.5,27.5,26.5)
##     index17 <- subset(index,
##                       section=list(longitude=lon, latitude=lat, width=50))
##     plot(index17, bathymetry=FALSE)
##     points(lon, lat, pch=21, col="black", bg="red", type="o", lwd=3)
##}
#'
#' # Subset to profiles with oxygen data
#' indexOxygen <- subset(indexSynthetic, parameter="DOXY")
#'
#' # Subset to profiles with both salinity and 380-nm downward irradiance data
#' indexSalinityIrradiance <- subset(indexSynthetic, parameter=c("PSAL", "DOWN_IRRADIANCE380"))
#'
#' @references
#' 1. Carval, Thierry, Bob Keeley, Yasushi Takatsuki, Takashi Yoshida, Stephen Loch,
#' Claudia Schmid, and Roger Goldsmith. Argo User's Manual V3.3. Ifremer, 2019.
#' `doi:10.13155/29825`.
#' 2. Kelley, D. E., Harbin, J., & Richards, C. (2021). argoFloats: An R package for analyzing
#' Argo data. Frontiers in Marine Science, (8), 636922.
#' \doi{10.3389/fmars.2021.635922}
#'
#' @importFrom stats approx
#'
#' @importFrom oce geodDist
#'
#' @export
#'
#' @author Dan Kelley and Jaimie Harbin
setMethod(f="subset",
    signature="argoFloats",
    definition=function(x, subset=NULL, ...)
    {
        if (x@metadata$type == "profiles")
            stop("in subset,argoFloats-method() :\n  subset doesn't work for type = profiles", call.=FALSE)
        ## subsetString <- paste(deparse(substitute(subset)), collapse=" ")
        dots <- list(...)
        dotsNames <- names(dots)
        ## Clear the "debug" and "silent" entries from dots, because later we insist that length(dots) be 1.
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
        ## Step 1: handle "argo" type first. Note that "subset" is ignored; rather, we insist that either
        ## "profile" or "cycle" be provided.
        if (x@metadata$type == "argos") {
            N <- x[["length"]]
            if (!missing(subset)) {
                N <- x[["length"]]
                if (is.logical(subset)) {
                    if (length(subset) != x[["length"]])
                        stop("in subset,argoFloats-method() :\n  'subset' length (", length(subset), ") must match x length (", x[["length"]], ")",
                            call.=FALSE)
                    keep <- which(subset)
                } else if (is.numeric(subset)) {
                    if (!all(subset > 0))
                        stop("in subset,argoFloats-method() :\n  'subset' must consist only of positive integers",
                            call.=FALSE)
                    if (any(subset > x[["length"]]))
                        stop("in subset,argoFloats-method() :\n  'subset' must not contain indices beyond range of `x` (in this case, ", x[["length"]], ")",
                            call.=FALSE)
                    keep <- subset
                } else {
                    stop("in subset,argoFloats-method() :\n  'subset' must either be a logical vector or a list of indices",
                        call.=FALSE)
                }
                res <- x
                res@data$argos <- x@data$argos[keep]
                if (!silent)
                    message("Kept ", length(keep), " cycles (", sprintf("%.3g", 100*length(keep)/N), "%)")
                argoFloatsDebug(debug, "} # subset,argoFloats-method()\n", style="bold", sep="", unindent=1)
                return(res)
            }
            argoFloatsDebug(debug, "subsetting with type=\"argos\"\n")
            if (length(dotsNames) == 0)
                stop("in subset,argoFloats-method() :\n  must give \"profile\" , \"cycle\", or \"dataStateIndicator\" argument", call.=FALSE)
            if (dotsNames[1] == "profile") {
                argoFloatsDebug(debug, "subsetting by profile ", profile, "\n")
                profile <- dots[[1]]
                ## Loop over all objects within the data, and within that loop look at data within the object,
                ## and for each of them, if its a vactor subset according to profile and if its a matrix
                ## subset according to profile
                res <- x
                argos <- x[["argos"]]
                ## Loop over all objects
                ##message("profile=",paste(profile, collapse=" "))
                for (iargo in seq_along(argos)) {
                    argo <- argos[[iargo]]
                    ## Handle the metadata slot
                    for (name in names(argo@metadata)) {
                        ##message("name=",name)
                        argoFloatsDebug(debug, "subsetting metadata item named \"", name, "\"\n", sep="")
                        ## Pass some things through directly.
                        if (name %in% c("units", "filename", "flagScheme", "dataNamesOriginal"))
                            next
                        item <- argo@metadata[[name]]
                        ## Handle things that are encoded as characters in a string,
                        ## namely "direction", "juldQC", and "positionQC".
                        if (name == "direction" || grepl("QC$", name)) {
                            ## message("  -- character")
                            res@data$argos[[iargo]]@metadata[[name]] <- paste(strsplit(item,"")[[1]][profile],collapse="")
                        } else if (is.list(item)) {
                            ##message("list")
                            for (l in seq_along(item)) {
                                ##print(dim(item[[l]]))
                                D <- dim(item[[l]])
                                if (profile > D[2])
                                    stop("in subset,argoFloats-method() :\n cannot access profile ", profile, " of metadata item \"", name, "\" because its dimension is ", paste(D, collapse=" "), call.=FALSE)
                                ##cat("BEFORE:\n");print(dim(res@data$argos[[iargo]]@metadata[[name]][[l]]))
                                res@data$argos[[iargo]]@metadata[[name]][[l]] <- item[[l]][, profile, drop=FALSE]
                                ##cat("AFTER:\n");print(dim(res@data$argos[[iargo]]@metadata[[name]][[l]]))
                            }
                        } else if (is.vector(name)) {
                            ##message("vector")
                            res@data$argos[[iargo]]@metadata[[name]] <- item[profile]
                        } else if (is.matrix(name)) {
                            ##message("matrix")
                            res@data$argos[[iargo]]@metadata[[name]] <- item[, profile, drop=FALSE]
                        } else if (is.array(name)) {
                            argoFloatsDebug(debug, "name=", name, " has dim ", paste(dim(res@metadata[[name]]), collapse=" "), "\n")
                            if (length(dim(res@metadata[[name]])) <= 3) {
                                res@metadata[[name]] <- item[, , keep, drop=FALSE]
                            } else {
                                warning("not subsetting \"", name, "\" in metadata, because it is an array of rank > 3")
                            }
                        } else {
                            stop("cannot subset metadata item named \"", name, "\" because it is not a length-one string, a vector, or a matrix")
                        }
                    }
                    ## Handle the data slot
                    for (name in names(argo@data)) {
                        item <- argo@data[[name]]
                        if (is.matrix(item)) {
                            dim <- dim(item)
                            if (profile > dim[2])
                                stop("in subset,argoFloats-method() :\n  Only have ", dim[2], " profiles", call.=FALSE)
                            newItem <- item[, profile, drop=FALSE]
                            res@data$argos[[iargo]]@data[[name]] <- newItem
                        } else {
                            length <- length(item)
                            if (profile > length)
                                stop("in subset,argoFloats-method() :\n  Only have ", length, " profiles", call.=FALSE)
                            newItem <- item[profile, drop=FALSE]
                            res@data$argos[[iargo]]@data[[name]] <- newItem
                        }
                    }
                }
                argoFloatsDebug(debug, "} # subset,argoFloats-method()\n", style="bold", sep="", unindent=1)
                res@processingLog <- oce::processingLogAppend(res@processingLog,
                                                  paste("subset argos type for profile=", profile))
                return(res)
            } else if (dotsNames[1] == "ID") {
                istraj <- identical(x@metadata$subtype, "trajectories")
                argoFloatsDebug(debug, "subsetting an argos object by ID\n")
                ID <- as.character(dots[[1]]) # convert in case it is numeric
                xID <- x[["ID"]]
                keep <- rep(FALSE, length(xID))
                for (thisID in ID)
                    keep <- keep | grepl(thisID, xID)
                if (!silent && !istraj) {
                    message("Kept ", sum(keep), " cycles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                } else if (!silent && istraj) {
                    message("Kept ", sum(keep), " trajectories (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                }
                x@data$argos <- x@data$argos[keep]
                x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                            paste("subset argos type by float ID", ID))
            } else if (dotsNames[1] == "cycle") {
                argoFloatsDebug(debug, "subsetting by cycle for 'argos' type\n")
                cycle <- dots[[1]]
                if (!is.character(cycle) & !is.numeric(cycle))
                    stop("in subset,argoFloats-method() : \"cycle\" must be character value or numeric value", call.=FALSE)
                ## Calculate 'keep', a logical vector that will be used for the actual subsetting.
                xcycle <- x[["cycle"]]
                argoFloatsDebug(debug, "x[[\"cycle\"]]: ", paste(xcycle, collapse=" "), "\n")
                argoFloatsDebug(debug, "cycle: ", cycle, "\n")
                if (is.character(cycle)) {
                    argoFloatsDebug(debug, "subsetting by cycle as a character value\n")
                    ## Here, keep is logical
                    keep <- rep(FALSE, length(xcycle))
                    for (thisCycle in cycle)
                        keep <- keep | grepl(thisCycle, xcycle)
                    nkeep <- sum(keep)
                } else if (is.numeric(cycle)) {
                    argoFloatsDebug(debug, "subsetting by cycle as a numeric value\n")
                    ## Here, keep is integer
                    keep <- NULL
                    xcycle <- as.integer(gsub("AD","",xcycle)) # change e.g. "123D" to "123"
                    for (thisCycle in cycle) {
                        keep <- c(keep, which(thisCycle == xcycle))
                        ## message("thisCycle=", thisCycle, " keep=", paste(keep, collapse=" "))
                    }
                    nkeep <- length(keep)
                }
                if (nkeep < 1)
                    warning("In subset,argoFloats-method(..., parameter) : found no profiles with given cycle(s)", call.=FALSE)
                if (!silent)
                    message("Kept ", nkeep, " cycles ")
                x@data[[1]] <- x@data[[1]][keep]
                x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste("subset argos type for cycle=", cycle))
            } else if (dotsNames[1] == "dataStateIndicator") {
                argoFloatsDebug(debug, "subsetting by dataStateIndicator\n")
                dataStateIndicator <- dots[[1]]
                if (is.list(dots[1]))
                    dataStateIndicator <- unlist(dataStateIndicator)
                ## Reference Table 6, in section 3.6, of
                ## Argo Data Management Team. “Argo User's Manual V3.3.”
                ## Ifremer, November 28, 2019. https://doi.org/10.13155/29825.
                if (!is.character(dataStateIndicator))
                    stop("in subset,argoFloats-method() :  \"dataStateIndicator\" must be character value", call.=FALSE)
                ## In next, [1] means we take the first element (first profile) of the cycle.
                dsi <- sapply(x[["argos"]], function(a) a[["dataStateIndicator"]][1])
                keep <- rep(FALSE, length(dsi))
                for (dataStateIndicatorThis in dataStateIndicator) {
                    keep <- keep | grepl(dataStateIndicatorThis, dsi , fixed=TRUE)
                    ##> message(dataStateIndicatorThis)
                    ##> print(keep)
                }
                x@data[[1]] <- x@data[[1]][keep]
                x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste("subset argos type by dataStateIndicator=", dataStateIndicator))
            } else if (dotsNames[1] == "dataMode") {
                seeking <- dots$dataMode
                # This testing of 'dataMode' may be overly complex
                if (length(seeking) != 1)
                    stop("in subset,argoFloats-method(): length of 'dataMode' must be 1", call.=FALSE)
                if (!seeking %in% c("realtime", "adjusted", "delayed"))
                    stop('in subset,argoFloats-method(): \'dataMode\' must be "realtime", "adjusted" or "delayed", not "', seeking, '"', call.=FALSE)
                seeking <- switch(seeking, realtime="R", adjusted="A", delayed="D")
                res <- x
                a <- x@data$argos
                for (i in seq_along(a)) {
                    A <- a[[i]]
                    keepProfile <- A[["dataMode"]] == seeking
                    #> message("i=", i, ", keepProfile=", paste(keepProfile, collapse=" "))
                    ## flags
                    for (flagName in names(A@metadata$flags)) {
                        A@metadata$flags[[flagName]] <- A@metadata$flags[[flagName]][, keepProfile, drop=FALSE]
                    }
                    # dataMode
                    A@metadata$dataMode <- A@metadata$dataMode[keepProfile, drop=FALSE]
                    # data
                    for (dataName in names(A@data)) {
                        if (is.vector(A@data[[dataName]])) {
                            #> message("subset ", dataName, " vector from ", paste(A@data[[dataName]], collapse=" "))
                            A@data[[dataName]] <- A@data[[dataName]][keepProfile]
                            #> message("   ... to ", paste(A@data[[dataName]], collapse=" "))
                        } else if (is.array(A@data[[dataName]])) {
                            A@data[[dataName]] <- A@data[[dataName]][, keepProfile, drop=FALSE]
                        }
                    }
                    res@data$argos[[i]] <- A
                }
                res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
                argoFloatsDebug(debug, "} # subset,argoFloats-method()\n", style="bold", sep="", unindent=1)
                return(res)
            } else {
                stop("in subset,argoFloats-method():\n  the \"...\" argument \"", dotsNames[1], "\" is not permitted for an argos-type object. The only valid choices are \"cycle\", \"dataMode\", \"ID\", \"dataSateIndicator\" and \"profile\".", call.=FALSE)
            }
        }
        ## Step 2: Now we know the type is either "index" or "profiles".  In either case,
        ## "subset" can be provided, so we check for its existence first.
        if (missing(subset)) {
            #argoFloatsDebug(debug, "no subset was given, so it must be circle=, rectangle=, or similar\n")
            if (length(dots) == 0)
                stop("in subset,argoFloats-method() :\n for indices, must specify the subset, with \"subset\" argument, \"circle\",\"rectangle\", \"parameter\",\"polygon\", \"time\", \"institution\", \"deep\", \"ID\", \"ocean\", dataMode\", \"cycle\",\"direction\", or \"section\"", call.=FALSE)
            if (length(dots) > 1)
                stop("in subset,argoFloats-method() :\n  cannot give more than one method in the \"...\" argument", call.=FALSE)
            N <- length(x@data$index[[1]]) # used in calculating percentages
            if (x@metadata$type == "index") {
                istraj <- identical(x@metadata$subtype, "trajectories")
                if (dotsNames[1] == "circle" && !istraj) {
                    argoFloatsDebug(debug, "subsetting an index by circle\n")
                    circle <- dots[[1]]
                    if (!is.list(dots[1]))
                        stop("in subset,argoFloats-method() :\n  \"circle\" must be a list containing \"longitude\", \"latitude\" and \"radius\".", call.=FALSE)
                    if (3 != sum(c("longitude", "latitude", "radius") %in% sort(names(circle))))
                        stop("in subset,argoFloats-method() :\n  \"circle\" must be a list containing \"longitude\", \"latitude\" and \"radius\"", call.=FALSE)
                    if (!requireNamespace("oce", quietly=TRUE))
                        stop("must install.packages(\"oce\") to subset by circle")
                    dist <- oce::geodDist(x[["longitude"]], x[["latitude"]], circle$longitude, circle$latitude)
                    keep <- dist < circle$radius
                    keep[is.na(keep)] <- FALSE
                    x@data$index <- x@data$index[keep, ]
                    x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste("subset index type by circle with longitude= ", circle$longitude, " , latitude= ", circle$latitude, ", and radius= ", circle$radius))
                    if (!silent)
                        message("Kept ", sum(keep), " cycles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                } else if (dotsNames[1] == "circle" && istraj) {
                        stop("in subset,argoFloats-method(): cannot subset circle for \"index\" type with subtype = trajectories", call.=FALSE)
                } else if (dotsNames[1] == "rectangle") {
                    argoFloatsDebug(debug, "subsetting an index by rectangle\n")
                    rectangle <- dots[[1]]
                    if (!is.list(dots[1]))
                        stop("in subset,argoFloats-method():\n  \"rectangle\" must be a list containing \"longitude\" and \"latitude\"", call.=FALSE)
                    if (2 != sum(c("longitude", "latitude") %in% sort(names(rectangle))))
                        stop("in subset,argoFloats-method():\n  \"rectangle\" must be a list containing \"longitude\" and \"latitude\"", call.=FALSE)
                    if (!istraj) {
                        keeplon <- rectangle$longitude[1] <=x[["longitude"]] & x[["longitude"]] <= rectangle$longitude[2]
                        keeplat <- rectangle$latitude[1] <= x[["latitude"]] & x[["latitude"]] <= rectangle$latitude[2]
                        ok <- is.finite(keeplon) & is.finite(keeplat)
                        keeplon[!ok] <- FALSE
                        keeplat[!ok] <- FALSE
                        keep <- keeplon & keeplat
                    } else if (istraj) {
                        if (!requireNamespace("sf", quietly=TRUE))
                            stop("must install sf package for subset(...,circle,...) to work with trajectory files")
                        sE <- rectangle$longitude[2]
                        sW <- rectangle$longitude[1]
                        sS <- rectangle$latitude[1]
                        sN <- rectangle$latitude[2]
                        s <- rbind(c(sW,sS), c(sW, sN), c(sE,sN), c(sE, sS), c(sW,sS))
                        S <- sf::st_polygon(list(s))
                        n <- nrow(x@data$index)
                        keep <- rep(FALSE, n)
                        for (i in seq_len(n)) {
                            #message("i equals ", i)
                            #print(x@data$index[i,])
                            tE <- as.numeric(x@data$index$longitude_max[i])
                            tW <- as.numeric(x@data$index$longitude_min[i])
                            tS <- as.numeric(x@data$index$latitude_min[i])
                            tN <- as.numeric(x@data$index$latitude_max[i])
                            t <- rbind(c(tW,tS), c(tW, tN), c(tE,tN), c(tE, tS), c(tW,tS))
                            T <- sf::st_polygon(list(t))
                            intersection <- sf::st_intersection(T, S)
                            keep[i] <- length(intersection) > 0
                            #print(intersects)
                            #message('intersects ', length(intersects) > 0)
                        }
                    }
                    x@data$index <- x@data$index[keep, ]
                    x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                                paste("subset index type by rectangle with longitude= ", rectangle$longitude, " and latitude= ", rectangle$latitude ))
                    if (!silent)
                        message("Kept ", sum(keep), " cycles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                } else if (dotsNames[1] == "parameter") {
                    argoFloatsDebug(debug, "subsetting an index by parameter\n")
                    parameter <- dots[[1]]
                    if (is.list(dots[1]))
                        parameters <- unlist(parameter)
                    nparameters <- length(parameters)
                    parametersList <- lapply(x[["parameters"]], function(p) strsplit(p, " ")[[1]])
                    keep <- unlist(lapply(parametersList, function(pl) nparameters == sum(parameters %in% pl)))
                    #if (sum(keep) < 1)
                    #warning("in subset,argoFloats-method(..., parameter):\n  found no profiles with given parameter", call.=FALSE)
                    if (!silent && !istraj) {
                        message("Kept ", sum(keep), " cycles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                    } else if (!silent && istraj) {
                        message("Kept ", sum(keep), " trajectories (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                    }
                    x@data$index <- x@data$index[keep, ]
                    x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste("subset index type by parameter= ",parameter))
                } else if (dotsNames[1] == "polygon" && !istraj) {
                    argoFloatsDebug(debug, "subsetting an index by polygon\n")
                    if (!requireNamespace("sf", quietly=TRUE))
                        stop("must install.packages(\"sf\") for subset() by polygon to work")
                    polygon <- dots[[1]]
                    if(!is.list(dots[1]))
                        stop("in subset,argoFloats-method():\n  \"polygon\" must be a list", call.=FALSE)
                    if (length(polygon) != 2)
                        stop("in subset,argoFloats-method():\n  \"polygon\" must be a list of two elements", call.=FALSE)
                    if (2 != sum(c("longitude", "latitude") %in% names(polygon)))
                        stop("in subset,argoFloats-method():\n  \"polygon\" must be a list containing \"longitude\" and \"latitude\"", call.=FALSE)
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
                    ## points.  It is not too difficult to get the index with the sp
                    ## package, but the only solution I could come up with using the sf
                    ## package is to tack 1,2,3,etc onto the lon-lat points as a third
                    ## dimension, so that after we select for the points inside, we can skim
                    ## that third dimension and that gives us the keep value that we need. There
                    ## may be a more straightforward way, but my (admittedly shallow) reading
                    ## of the sf function list did not uncover anything promising, and my
                    ## tests show that this scheme works.
                    ##
                    ## See https://github.com/ArgoCanada/argoFloats/issues/86
                    ok <- is.finite(alon) & is.finite(alat)
                    if (!requireNamespace("sf", quietly=TRUE))
                        stop("must install sf package for subset(...,polygon,...) to work")
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
                        stop(paste0("Error in subset,argoFloats-method():\n  \"Points\" is invalid, because of ", errorMessage), call.=FALSE)
                    }
                    Intersection <- sf::st_intersection(Points, Polygon)
                    keep <- Intersection[,3]
                    if (!silent)
                        message("Kept ", length(keep), " cycles (", sprintf("%.3g", 100*length(keep)/N), "%)")
                    x@data$index <- x@data$index[keep, ]
                    x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste("subset index type by polygon"))
                } else if (dotsNames[1] == "polygon" && istraj) {
                        stop("in subset,argoFloats-method(): cannot subset polygon for \"index\" type with subtype = trajectories", call.=FALSE)
                } else if (dotsNames[1] == "time") {
                    argoFloatsDebug(debug, "subsetting an index by time\n")
                    time <- dots[[1]]
                    if(!is.list(dots[1]))
                        stop("in subset,argoFloats-method():\n  \"time\" must be a list", call.=FALSE)
                    if (2 != sum(c("from", "to") %in% names(time)))
                        stop("in subset,argoFloats-method():\n  \"time\" must be a list containing \"to\"and \"from\"", call.=FALSE)
                    if (length(time$from) != 1)
                        stop("from must be of length 1")
                    if (length(time$to) != 1)
                        stop("to must be of length 1")
                    if (!inherits(time$from, "POSIXt")) {
                        time$from <- try(as.POSIXct(time$from, tz="UTC"))
                        if (inherits(time$from, "try-error"))
                            stop("in subset,argoFloats-method():\n  cannot convert \"time$from\" to a POSIX time", call.=FALSE)
                    }
                    if (!inherits(time$to, "POSIXt")) {
                        time$to <- try(as.POSIXct(time$to, tz="UTC"))
                        if (inherits(time$to, "try-error"))
                            stop("in subset,argoFloats-method():\n  cannot convert \"time$to\" to a POSIX time", call.=FALSE)
                    }
                    if (time$to <= time$from)
                        stop ("in subset,argoFloats-method():\n \"to\" must be greater than \"from\"", call.=FALSE)
                    argoFloatsDebug(debug, "from= ", format(time$from, "%Y-%m-%d %H:%M:%S %z"), "\n")
                    argoFloatsDebug(debug, "to= ", format(time$to, "%Y-%m-%d %H:%M:%S %z"), "\n")
                    keep <- time$from[1] <= x[["date"]] & x[["date"]] <= time$to[1]
                    keep[is.na(keep)] <- FALSE
                    if (!silent && !istraj) {
                        message("Kept ", sum(keep), " cycles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                    } else if (!silent &&istraj) {
                        message("Kept ", sum(keep), " trajectories (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                    }
                    x@data$index <- x@data$index[keep, ]
                    x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste("subset index type by time from", time$from, "to ", time$to))
                } else if(dotsNames[1] == "institution") {
                    argoFloatsDebug(debug, "subsetting an index by institution\n")
                    institution <- dots[[1]]
                    if(!is.list(dots[1]))
                        stop("in subset,argoFloats-method():\n  \"institution\" must be a list")
                    if (length(institution) > 1)
                        stop("\"institution\" cannot hold more than one element")
                    keep <- grepl(institution, x@data$index$institution)
                    keep[is.na(keep)] <- FALSE
                    if (!silent && !istraj) {
                        message("Kept ", sum(keep), " cycles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                    } else if (!silent && istraj) {
                        message("Kept ", sum(keep), " trajectories (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                    }
                    x@data$index <- x@data$index[keep, ]
                    x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste("subset index type by institution " ,institution))
                } else if (dotsNames[1] == "deep") {
                    argoFloatsDebug(debug, "subsetting an index by deep category\n")
                    deep <- dots[[1]]
                    if (!is.logical(deep))
                        stop("in subset,argoFloats-method():\n deep must be a logical vector indicating TRUE or FALSE", call.=FALSE)
                    if (deep) {
                        keep <- grep("849|862|864", x@data$index$profiler_type)
                    } else {
                        keep <- grep("849|862|864", x@data$index$profiler_type, invert=TRUE)
                    }
                    if (!silent && !istraj) {
                        message("Kept ", length(keep), " cycles (", sprintf("%.3g", 100*length(keep)/N), "%)")
                    } else if (!silent && istraj) {
                        message("Kept ", length(keep), " trajectories (", sprintf("%.3g", 100*length(keep)/N), "%)")
                    }
                    x@data$index <- x@data$index[keep, ]
                    x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste("subset index type to only include deep data"))
                } else if (dotsNames[1] == "ID") {
                    argoFloatsDebug(debug, "subsetting an index by ID\n")
                    ID <- as.character(dots[[1]]) # convert in case it is numeric
                    xID <- x[["ID"]]
                    keep <- rep(FALSE, length(xID))
                    file <- x@data$index$file
                    for (thisID in ID)
                        keep <- keep | grepl(thisID, xID)
                    if (!silent && !istraj) {
                        message("Kept ", sum(keep), " cycles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                    } else if (!silent && istraj) {
                        message("Kept ", sum(keep), " trajectories (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                    }
                    x@data$index <- x@data$index[keep, ]
                    x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste("subset index type by float ID", ID))
                } else if (dotsNames[1] == "ocean" && !istraj) {
                    argoFloatsDebug(debug, "subsetting an index by ocean\n")
                    ocean <- dots[[1]]
                    if (!is.character(ocean))
                        stop("in subset,argoFloats-method() : \"ocean\" must be character value", call.=FALSE)
                    if (length(ocean) > 1)
                        stop("in subset,argoFloats-method():\n \"ocean\" cannot hold more than one element", call.=FALSE)
                    keep <- grepl(ocean, x@data$index$ocean)
                    keep[is.na(keep)] <- FALSE
                    if (!silent)
                        message("Kept ", sum(keep), " cycles (", sprintf("%.3g", 100.0*sum(keep)/N), "%)")
                    x@data$index <- x@data$index[keep, ]
                    x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste("subset index type by ocean", ocean))
                } else if (dotsNames[1] == "ocean" && istraj) {
                        stop("in subset,argoFloats-method(): cannot subset ocean for \"index\" type with subtype = trajectories", call.=FALSE)
                } else if (dotsNames[1] == "dataMode") {
                    argoFloatsDebug(debug, "subsetting an index by dataMode\n")
                    dataMode <- dots[[1]]
                    if (!is.character(dataMode))
                        stop("in subset,argoFloats-method():\n  \"dataMode\" must be character value",call.=FALSE)
                    if (dataMode == "delayed" && !istraj) {
                        keep <- grepl("^[a-z]*/[0-9]*/profiles/.{0,1}D.*$", x[["file"]])
                    } else if (dataMode == "delayed" && istraj) {
                        keep <- grepl("^[a-z]*/[0-9]*/[0-9]*_.{0,1}D.*$", x[["file"]])
                    } else if (dataMode == "realtime" && !istraj) {
                        keep <- grepl("^[a-z]*/[0-9]*/profiles/.{0,1}R.*$", x[["file"]])
                    } else if (dataMode == "realtime" && istraj) {
                        keep <- grepl("^[a-z]*/[0-9]*/[0-9]*_.{0,1}R.*$", x[["file"]])
                    } else {
                        stop("in subset,argoFloats-method():\n  \"dataMode\" must be either \"realtime\" or \"delayed\", not \"", dataMode, "\"", call.=FALSE)
                    }
                    if (!silent && !istraj) {
                        message("Kept ", sum(keep), " cycles (", sprintf("%.3g", 100.0*sum(keep)/N), "%)")
                    } else if (!silent && istraj) {
                        message("Kept ", sum(keep), " trajectories (", sprintf("%.3g", 100.0*sum(keep)/N), "%)")
                    }

                    x@data$index <- x@data$index[keep, ]
                    x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste("subset index type by dataMode", dataMode))
                } else if (dotsNames[1] == "cycle" && !istraj) {
                    cycle <- dots[[1]]
                    if (!is.character(cycle) & !is.numeric(cycle))
                        stop("in subset,argoFloats-method() : \"cycle\" must be character value or numeric value", call.=FALSE)
                    ## Calculate 'keep', a logical vector that will be used for the actual subsetting.
                    xcycle <- x[["cycle"]]
                    # If cycle is numeric, we must convert it to character (so e.g. 1 becomes "001")
                    if (is.numeric(cycle)) {
                        cycle <- sprintf("%03d", as.integer(cycle))
                        argoFloatsDebug(debug, "subsetting an index by cycle of numeric type\n")
                    } else if (is.character(cycle)) {
                        argoFloatsDebug(debug, "subsetting an index by cycle of character type\n")
                    } else {
                        stop("cycle must be character or numeric")
                    }
                    keep <- rep(FALSE, length(xcycle))
                    for (thisCycle in cycle)
                        keep <- keep | grepl(thisCycle, xcycle)
                    nkeep <- sum(keep)
                    if (nkeep < 1)
                        warning("In subset,argoFloats-method(..., parameter) : found no profiles with given cycle(s)", call.=FALSE)
                    if (!silent)
                        message("Kept ", nkeep, " cycles (", sprintf("%.3g", 100*nkeep/N), "%)")
                    x@data$index <- x@data$index[keep, ]
                    x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste("subset index type by cycle=", cycle))
                } else if (dotsNames[1] == "cycle" && istraj) {
                        stop("in subset,argoFloats-method(): cannot subset cycle for \"index\" type with subtype = trajectories", call.=FALSE)
                } else if (dotsNames[1] == "direction" && !istraj) {
                    argoFloatsDebug(debug, "subsetting an index by direction\n")
                    direction <- dots[[1]]
                    if (!is.character(direction))
                        stop("in subset,argoFloats-method():\n  \"direction\" must be character value of either \"ascent\" or \"descent\"", call.=FALSE)
                    if (direction == "ascent") {
                        keep <- grepl("^.*[^D].nc$", x@data$index$file)
                    } else if (direction == "descent") {
                        keep <- grepl("^.*D.nc$", x@data$index$file)
                    } else {
                        stop("in subset,argoFloats-method():\n  \"direction\" must be either \"ascent\" or \"descent\", not \"", direction, "\"", call.=FALSE)
                    }
                    x@data$index <- x@data$index[keep, ]
                    x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste("subset index type by direction=", direction))
                } else if (dotsNames[1] == "direction" && istraj) {
                        stop("in subset,argoFloats-method(): cannot subset direction for \"index\" type with subtype = trajectories", call.=FALSE)
                } else if (dotsNames[1] == "section" && !istraj) {
                    # The following link shows a test of this method.
                    # https://github.com/ArgoCanada/argoFloats/issues/397#issuecomment-798960312
                    if (!requireNamespace("s2", quietly=TRUE))
                        stop("must install.packages(\"s2\") to subset by section")
                    argoFloatsDebug(debug, "subsetting an index by section\n")
                    section <- dots[[1]]
                    if (!is.list(section))
                        stop("in subset,argoFloats-method() :\n  section must be a list containing \"longitude\", \"latitude\" and possibly \"width\" and \"segments\"", call.=FALSE)
                    # Handle mandatory components
                    if (!"longitude" %in% names(section))
                        stop("in subset,argoFloats-method() :\n  section must have a \"longitude\" entry", call.=FALSE)
                    if (!"latitude" %in% names(section))
                        stop("in subset,argoFloats-method() :\n  section must have a \"latitude\" entry", call.=FALSE)
                    if (length(section$longitude) != length(section$latitude))
                        stop("in subset,argoFloats-method() :\n  length of section$longitude (",
                             length(section$longitude), ") and section$latitude (", length(section$latitude), ") disagree", call.=FALSE)
                    # Handle optional components
                    if (!"width" %in% names(section))
                        section$width <- 100
                    if (!"segments" %in% names(section))
                        section$segments <- 100
                    if (!is.null(section$segments)) {
                        if (!is.numeric(section$segments) || section$segments > 0) {
                            argoFloatsDebug(debug, "subdividing lon,lat trace (of length ", length(section$longitude), ") to increase length to ", section$segments, "\n", sep="")
                            s <- seq(0, 1, length.out=length(section$longitude))
                            section$longitude <- stats::approx(s, section$longitude, seq(0, 1, length.out=section$segments))$y
                            section$latitude <- stats::approx(s, section$latitude, seq(0, 1, length.out=section$segments))$y
                        } else {
                            stop("in subset,argoFloats-method() :\n  section$segments must be NULL or a positive integer", call.=FALSE)
                        }
                    }
                    # Create a spine for the section.
                    sectionSpine <- s2::s2_make_line(section$longitude, section$latitude)
                    # Create points for float locations.  Blank the NAs, in case not allowed.
                    alon <- x[["longitude"]]
                    alat <- x[["latitude"]]
                    bad <- !is.finite(alon) | !is.finite(alat)
                    alon[bad] <- 0
                    alat[bad] <- 0
                    floatLocations <- s2::as_s2_geography(s2::s2_lnglat(alon, alat))
                    # Identify floats near the section spine
                    keep <- s2::s2_dwithin(floatLocations, sectionSpine, 1000*section$width)
                    argoFloatsDebug(debug, "sum(keep)=", sum(keep), " i.e. ",
                                    round(100*sum(keep)/length(keep), 3), "% (before accounting for NA values)\n", sep="")
                    keep <- keep & !bad
                    argoFloatsDebug(debug, "sum(keep)=", sum(keep), " i.e. ",
                                    round(100*sum(keep)/length(keep), 3), "% (after accounting for NA values)\n", sep="")
                    if (!silent)
                        message("Kept ", sum(keep), " cycles (", sprintf("%.3g", 100*sum(keep)/N), "%)")
                    x@data$index <- x@data$index[keep, ]
                    x@processingLog <- oce::processingLogAppend(x@processingLog,
                                                  paste0("subset index type by section with longitude=c(",
                                                        paste(section$longitude, collapse=", "), "), latitude=c(",
                                                        paste(section$latitude, collapse=", "), "), and width=",
                                                        section$width, " km"))
                } else if (dotsNames[1] == "section" && istraj) {
                        stop("in subset,argoFloats-method(): cannot subset section for \"index\" type with subtype = trajectories", call.=FALSE)
                } else {
                    stop("in subset,argoFloats-method():\n  the \"...\" argument \"", dotsNames[1], "\" is not permitted for an index-type object. The only valid choices are \"circle\", \"rectangle\", \"parameter\", \"polygon\", \"time\", \"institution\", \"deep\", \"ID\", \"ocean\", \"dataMode\", \"cycle\",\"direction\" and \"section\"", call.=FALSE)
                }
            }
        } else {
            ## subset is not missing
            if (length(dotsNames) != 0)
                stop("in subset,argoFloats-method():\n  cannot give both \"subset\" and \"...\" arguments", call.=FALSE)
            if (x@metadata$type == "index") {
                if (!silent) {
                    if (is.logical(subset)) # this simplifies the percentage count for the method
                        subset <- which(subset)
                    message("Kept ", length(subset), " cycles (", sprintf("%.3g", 100.0*length(subset)/dim(x@data$index)[1]), "%)")
                }
                x@data$index <- x@data$index[subset, ]
            } else {
                stop("in subset,argoFloats-method():\n  method not coded except for type=\"index\"", call.=FALSE)
            }
        }
        argoFloatsDebug(debug, "} # subset,argoFloats-method()\n", style="bold", sep="", unindent=1)
        x
    })


