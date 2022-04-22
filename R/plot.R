## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

colDefaults <- list(core="7", bgc="#05f076", deep="6")

#' Plot an argoFloats Object
#'
#' The action depends on the `type` of the object, and
#' this is set up by the function that created the object;
#' see \dQuote{Details}. These are basic plot styles, with
#' somewhat limited scope for customization. Since the data with
#' [argoFloats-class] objects are easy to extract, users should
#' not find it difficult to create their own plots to meet a
#' particular aesthetic. See \dQuote{Examples} and Kelley et al. (2021)
#' for more plotting examples.
#'
#' The various plot types are as follows.
#'
#' * For `which="map"`, a map of profile locations is created if subtype
#' is equal to cycles, or a rectangle highlighting the trajectory of a
#' float ID is created when subtype is equal to trajectories. This
#' only works if the `type` is `"index"` (meaning that `x` was created
#' by [getIndex()] or a subset of such an object, created with
#' [subset,argoFloats-method()]), or `argos` (meaning that
#' `x` was created with [readProfiles()].  The plot range is
#' auto-selected.  If the `ocedata` package
#' is available, then its `coastlineWorldFine` dataset is used to draw
#' a coastline (which will be visible only if the plot region
#' is large enough); otherwise, if the `oce` package is available, then its
#' `coastlineWorld` dataset is used.
#' The `bathymetry` argument controls whether (and how) to draw a map underlay
#' that shows water depth. There are three possible values for `bathymetry`:
#'     1. `FALSE` (the default, via [argoDefaultBathymetry()]), meaning not to draw bathymetry;
#'     2. `TRUE`, meaning to draw bathymetry as an
#'        image, using data downloaded with [oce::download.topo()].
#'        Example 4 illustrates this, and also shows how to adjust
#'        the margins after the plot, in case there is a need to add
#'        extra graphical elements using [points()], [lines()], etc.
#'     3. A list with items controlling both the bathymetry data and its
#'        representation in the plot (see Example 4).  Those items are:
#'
#'         1. `source`, a mandatory value that is either
#'            (a) the string `"auto"` (the default) to use
#'            [oce::download.topo()] to download the data
#'            or (b) a value returned by [oce::read.topo()].
#'         2. `contour`, an optional logical value (with `FALSE` as the default)
#'            indicating whether to represent
#'            bathymetry with contours (with depths of 100m, 200m, 500m shown,
#'            along with 1km, 2km up to 10km), as opposed to an image;
#'         3. `colormap`, ignored if `contour` is `TRUE`,
#'            an optional value that is either the string `"auto"` (the default)
#'            for a form of GEBCO colors computed with
#'            [oce::oceColorsGebco()], or a value computed with [oce::colormap()]
#'            applied to the bathymetry data; and
#'         4. `palette`, ignored if `contour` is `TRUE`,
#'            an optional logical value (with `TRUE` as the default)
#'            indicating whether to draw a depth-color
#'            palette to the right of the plot.
#' Note that the default value for `bathymetry` is set by a call
#' to [argoDefaultBathymetry()], and that this second function can only handle possibilities
#' 1 and 2 above.
#'
#' * For `which="profile"`, a profile plot is created, showing the variation of
#' some quantity with pressure or potential density anomaly, as specified by the
#' `profileControl` argument.
#'
#' * For `which="QC"`, two time-series panels are shown, with
#' time being that recorded in the individual profile in the dataset.
#' An additional argument named `parameter` must be given, to name the
#' quantity of interest.  The function only works if `x` is an
#' [`argoFloats-class`] object created with [readProfiles()].
#' The top panel shows the percent of data flagged with codes
#' 1 (meaning good data), 2 (probably good), 5 (changed)
#' or 8 (estimated), as a function of time (lower axis) and
#' (if all cycles are from a single Argo float)
#' cycle number (upper axis, with smaller font).
#' Thus, low values on the top panel reveal
#' profiles that are questionable. Note that if all of data at a given time
#' have flag 0, meaning not assessed, then a quality of 0 is plotted at that
#' time. The bottom panel shows the mean value of the parameter in question
#' regardless of the flag value.
#'
#' * For `which="summary"`, one or more time-series panels are shown
#' in a vertical stack.  If there is only one ID in `x`, then the cycle
#' values are indicated along the top axis of the top panel. The choice
#' of panels is set by the `summaryControl` argument.
#'
#' * For `which="TS"`,  an overall TS plot is created.  This only works if `x`
#' is an [argoFloats-class] object of type `"argos"`, i.e. if it was
#' created by [readProfiles()]. The scales for the plot
#' can be altered by putting `Slim` and `Tlim` arguments in the `...` list; see
#' the documentation for [oce::plotTS()] for other arguments that can be
#' provided. This plot has a default color code to represent bad, good,
#' and unassessed data.
#' This scheme comes from sections 3.2.1 and 3.2.2 of Carval et al. (2019), in which
#' data are considered bad if flagged 3, 4, 6, or 7, good
#' if flagged 1, 2, 5, or 8, and not accessed if flagged 0; good values are plotted
#' with black symbols, bad ones are plotted with red symbols, and not assessed values
#' are plotted with gray symbols.
#'
#' @param x an [`argoFloats-class`] object.
#'
#' @param which a character value indicating the type of plot. The possible
#' choices are `"map"`, `"profile"`, `"QC"`, `"summary"` and `"TS"`;
#' see \dQuote{Details}.
#'
#' @param bathymetry an argument used only if `which="map"`, to control
#' whether (and how) to indicate water depth. Note that the default
#' was `TRUE` until 2021-12-02, but was changed to `FALSE` on that
#' date, to avoid a bathymetry download, which can be a slow operations.
#' See \dQuote{Details} for details,
#' and Example 4 for a hint on compensating for the margin
#' adjustment done if an image is used to show bathymetry.
#'
#' @param geographical flag indicating the style of axes
#' for the `which="map"` case, but only if no projection is called
#' for in the `mapControl` argument.  With `geographical=0` (which
#' is the default), the axis ticks are labeled with signed
#' longitudes and latitudes, measured in degrees. The signs are dropped
#' with `geographical=1`.
#' In the `geographical=4` case, signs are also dropped, but hemispheres
#' are indicated by writing `S`, `N`, `W` or `E` after axis tick labels,
#' except at the equator and prime meridian.
#' Note that this scheme mimics that used by [oce::plot,coastline-method()].
#'
#' @param xlim,ylim numerical values, each a two-element vector, that
#' set the `x` and `y` limits of plot axes, as for [plot.default()] and
#' other conventional plotting functions.
#'
#' @param xlab a character value indicating the name for the horizontal axis, or
#' `NULL`, which indicates that this function should choose an appropriate name
#' depending on the value of `which`. Note that `xlab` is not obeyed if
#' `which="TS"`, because altering that label can be confusing to the user.
#'
#' @param ylab as `xlab`, but for the vertical axis.
#'
#' @param type a character value that controls the line type, with `"p"` for
#' unconnected points, `"l"` for line segments between undrawn points, etc.;
#' see the docs for [par()], If `type` not specified, it defaults to `"p"`.
#'
#' @param cex a character expansion factor for plot symbols, or `NULL`, to get an
#' value that depends on the value of `which`.
#'
#' @param col the colour to be used for plot symbols, or `NULL`, to get an value
#' that depends on the value of `which` (see \dQuote{Details}).  If `which="TS"`,
#' then the `TSControl` argument takes precedence over `col`.
#'
#' @param bg the colour to be used for plot symbol interior, for `pch`
#' values that distinguish between the interior of the symbol and the
#' border, e.g. for `pch=21`.
#'
#' @param pch an integer or character value indicating the type of plot symbol,
#' or `NULL`, to get a value that depends on the value of `which`.
#' (See [par()] for more on specifying `pch`.)
#'
#' @param eos a character value indicating the equation of state to use
#' if `which="TS"`.  This must be `"gsw"` (the default) or `"unesco"`;
#' see [oce::plotTS()].
#'
#' @param mapControl a list that permits particular control of the `which="map"`
#' case.  If provided, it may contain elements named `bathymetry` (which
#' has the same effect as the parameter `bathymetry`), `colLand`
#' (which indicates the colour of the land), and `projection` (which
#' may be `FALSE`, meaning to plot longitude and latitude on rectilinear axes,
#' `TRUE`, meaning to plot with [oce::mapPlot()], using Mollweide projection that
#' is suitable mainly for world-scale views, or a character value that will be
#' supplied to [oce::mapPlot()].  If a projection is used, then the positions
#' of the Argo floats are plotted with [oce::mapPoints()], rather than with
#' [points()], and if the user wishes to locate points with mouse clicks,
#' then [oce::mapLocator()] must be used instead of [locator()].  If `bathymetry`
#' is not contained in `mapControl`, it defaults to `FALSE`, and if `projection`
#' is not supplied, it defaults to `FALSE`.  Note that `mapControl` takes
#' precedence over the `bathymetry` argument, if both are provided.
#' Also note that, at present, bathymetry cannot be shown with map projections.
#'
#' @param profileControl a list that permits control of the `which="profile"`
#' case.  If provided, it may contain elements named `parameter` (a character value
#' naming the quantity to plot on the x axis), `ytype` (a character value equal to
#' either `"pressure"` or `"sigma0"`) and `connect` (a logical value indicating
#' whether to skip across `NA` values if the `type` argument is `"l"`, `"o"`,
#' or `"b"`).
#' If `profileControl` is not provided, it defaults to
#' `list(parameter="temperature", ytype="pressure", connect=FALSE)`. Alternatively,
#' if `profileControl` is missing any of the three elements, then they are
#' given defaults as in the previous sentence.
#'
#' @param QCControl a list that permits control of the `which="QC"`
#' case.  If provided, it may contain an element named `parameter`, a character
#' value naming the quantity for which the quality-control information is
#' to be plotted, and an element named `dataStateIndicator`, a logical
#' value controlling whether to add a panel showing this quantity
#' (see Reference Table 6 of Carval et al, 2019 to learn more
#' about what is encoded in `dataStateIndicator`).
#' If not provided, `QCControl` defaults to
#' `list(parameter="temperature",dataStateIndicator=FALSE)`.
#'
#' @param summaryControl a list that permits control of the `which="summary"`.
#' If provided, it should contain an element named `items`, a character vector
#' naming the items to be shown.  The possible entries in this vector are
#' `"dataStateIndicator"` (see Reference Table 6 of Carval et al, 2019,
#' for more information on this quantity)), `"length"` (the number of levels
#' in the profile), `"deepest"` (the highest pressure recorded),
#' `"longitude"` and `"latitude"`. If `summaryControl` is not provided,
#' all of these will be shown.  If all the elements of `x` have the same
#' `ID`, then the top panel will have ticks on its top axis, indicating
#' the `cycle`.
#'
#' @param TSControl a list that permits control of the `which="TS"`
#' case, and is ignored for the other cases.
#' If `TSControl` is not supplied as an argument,
#' points will be coloured black if their quality-control flags indicate
#' good data, red if flags indicate bad data, and gray if flags are not accessed.
#' Otherwise, if `TSControl` contains a
#' vector element named `colByCycle`, then the `col` argument will be ignored,
#' and instead individual cycles will be coloured as dictated by successive
#' elements in `colByCycle`.
#'
#' @param debug an integer specifying the level of debugging.
#'
#' @param \dots extra arguments passed to the plot calls that are made
#' within this function.
#'
#' @examples
#' # Example 1: map profiles in index
#' library(argoFloats)
#' data(index)
#' plot(index)
#'
#' # Example 2: as Example 1, but narrow the margins and highlight floats
#' # within a circular region of diameter 100 km.
#' oldpar <- par(no.readonly=TRUE)
#' par(mar=c(2, 2, 1, 1), mgp=c(2, 0.7, 0))
#' plot(index)
#' lon <- index[["longitude"]]
#' lat <- index[["latitude"]]
#' near <- oce::geodDist(lon, lat, -77.06, 26.54) < 100
#' R <- subset(index, near)
#' points(R[["longitude"]], R[["latitude"]],
#'     cex=0.6, pch=20, col="red")
#' par(oldpar)
#'
#' # Example 3: TS of a built-in data file.
#' f <- system.file("extdata", "SR2902204_131.nc", package="argoFloats")
#' a <- readProfiles(f)
#' oldpar <- par(no.readonly=TRUE)
#' par(mar=c(3.3, 3.3, 1, 1), mgp=c(2, 0.7, 0)) # wide margins for axis names
#' plot(a, which="TS")
#' par(oldpar)
#'\donttest{
#' # Example 4: map with bathymetry. Note that par(mar) is adjusted
#' # for the bathymetry palette, so it must be adjusted again after
#' # the plot(), in order to add new plot elements.
#' # This example specifies a coarse bathymetry dataset that is provided
#' # by the 'oce' package.  In typical applications, the user will use
#' # a finer-scale dataset, either by using bathymetry=TRUE (which
#' # downloads a file appropriate for the plot view), or by using
#' # an already-downloaded file.
#' data(topoWorld, package="oce")
#' oldpar <- par(no.readonly=TRUE)
#' par(mar=c(2, 2, 1, 2), mgp=c(2, 0.7, 0)) # narrow margins for a map
#' plot(index, bathymetry=list(source=topoWorld))
#' # For bathymetry plots that use images, plot() temporarily
#' # adds 2.75 to par("mar")[4] so the same must be done, in order
#' # to correctly position additional points (shown as black rings).
#' par(mar=par("mar") + c(0, 0, 0, 2.75))
#' points(index[["longitude"]], index[["latitude"]],
#'     cex=0.6, pch=20, col="red")
#' par(oldpar)
#'
#' # Example 5. Simple contour version, using coarse dataset (ok on basin-scale).
#' # Hint: use oce::download.topo() to download high-resolution datasets to
#' # use instead of topoWorld.
#' oldpar <- par(no.readonly=TRUE)
#' par(mar=c(2, 2, 1, 1))
#' data(topoWorld, package="oce")
#' plot(index, bathymetry=list(source=topoWorld, contour=TRUE))
#' par(oldpar)
##
## # NOTE: removed this for CRAN submission, because it's very slow, not
## # especially informative, and would require using tempdir() for saving
## # the index, which is *not* what we want to illustrate since it goes
## # against the whole point of caching.
## # Example 5B. World view with Mollweide projection (Canada Day, 2020)
## jul1 <- subset(getIndex(), time=list(from="2020-09-01", to="2020-09-02"))
## par(mar=c(2, 2, 1, 1), mgp=c(2, 0.7, 0)) # narrow margins for a map
## plot(jul1, which="map", mapControl=list(projection=TRUE), bathymetry=FALSE,
##      pch=20, col=4, cex=0.75)
#'
#' # Example 6. Customized map, sidestepping plot,argoFloats-method().
#' lon <- topoWorld[["longitude"]]
#' lat <- topoWorld[["latitude"]]
#' asp <- 1/cos(pi/180*mean(lat))
#' # Limit plot region to float region.
#' xlim <- range(index[["longitude"]])
#' ylim <- range(index[["latitude"]])
#' # Colourize 1km, 2km, etc, isobaths.
#' contour(x=lon, y=lat, z=topoWorld[["z"]], xlab="", ylab="",
#'         xlim=xlim, ylim=ylim, asp=asp,
#'         col=1:6, lwd=2, levels=-1000*1:6, drawlabels=FALSE)
#' # Show land
#' data(coastlineWorldFine, package="ocedata")
#' polygon(coastlineWorldFine[["longitude"]],
#'         coastlineWorldFine[["latitude"]], col="lightgray")
#' # Indicate float positions.
#' points(index[["longitude"]], index[["latitude"]], pch=20)
#'
## # Example 7: TS plot for a particular Argo
## a <- readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats"))
## par(mar=c(3.3, 3.3, 1, 1), mgp=c(2, 0.7, 0)) # wide margins for axis names
## plot(a, which="TS")
##
## # NOTE: removed this for CRAN submission, because it's very slow, not
## # especially informative, and would require using tempdir() for saving
## # the index, which is *not* what we want to illustrate since it goes
## # against the whole point of caching.
## # Example 7: Temperature QC plot for 25 cycles of a float in the Arabian Sea
## ais <- getIndex(filename="synthetic")
## sub <- subset(subset(ais, ID='2902123'), 50:75)
## profiles <- getProfiles(sub)
## argos <- readProfiles(profiles)
## par(mar=c(3.3, 3.3, 2, 1), mgp=c(2, 0.7, 0)) # wide margins for axis names
## plot(argos, which="QC") # defaults to temperature
## plot(argos, which="QC", QCControl=list(parameter="salinity"))
## plot(argos, which="QC", QCControl=list(parameter="salinity",dataStateIndicator=TRUE))
#'
#' # Example 7: Temperature profile of the 131st cycle of float with ID 2902204
#' a <- readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats"))
#' oldpar <- par(no.readonly=TRUE)
#' par(mfrow=c(1, 1))
#' par(mgp=c(2, 0.7, 0))                  # mimic the oce::plotProfile() default
#' par(mar=c(1,3.5,3.5,2))                # mimic the oce::plotProfile() default
#' plot(a, which="profile")
#' par(oldpar)
#'
#' # Example 8: As Example 7, but showing temperature dependence on potential density anomaly.
#' a <- readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats"))
#' oldpar <- par(no.readonly=TRUE)
#' par(mgp=c(2, 0.7, 0))                  # mimic the oce::plotProfile() default
#' par(mar=c(1,3.5,3.5,2))                # mimic the oce::plotProfile() default
#' plot(a, which="profile", profileControl=list(parameter="temperature", ytype="sigma0"))
#' par(oldpar)
#'
## # NOTE: removed this for CRAN submission, because it's very slow, not
## # especially informative, and would require using tempdir() for saving
## # the index, which is *not* what we want to illustrate since it goes
## # against the whole point of caching.
## # Example 10: Summary plot
## par(mar=c(2, 3.3, 2, 1))
## a <- readProfiles(getProfiles(subset(getIndex(), ID=1901584)))
## plot(a, which="summary")
#'}
#'
#' @references
#' 1. Carval, Thierry, Bob Keeley, Yasushi Takatsuki, Takashi Yoshida, Stephen Loch,
#' Claudia Schmid, and Roger Goldsmith. Argo User's Manual V3.3. Ifremer, 2019.
#' `doi:10.13155/29825`
#'
#' 2. Kelley, D. E., Harbin, J., & Richards, C. (2021). argoFloats: An R package for analyzing
#' Argo data. Frontiers in Marine Science, (8), 636922.
#' \doi{10.3389/fmars.2021.635922}
#'
#' @importFrom graphics abline axis box contour par plot.window points polygon rect text
#'
#' @importFrom grDevices extendrange gray rgb
#' @importFrom utils data
#' @importFrom oce as.ctd colormap drawPalette imagep oceColorsGebco oce.plot.ts plotTS
#'
#' @export
#'
#' @aliases plot,argoFloats-method
#'
#' @return None (invisible NULL).
#'
#' @author Dan Kelley and Jaimie Harbin
setMethod(f="plot",
          signature=signature("argoFloats"),
          definition=function(x,
                              which="map",
                              bathymetry=argoDefaultBathymetry(),
                              geographical=0,
                              xlim=NULL, ylim=NULL,
                              xlab=NULL, ylab=NULL,
                              type=NULL, cex=NULL, col=NULL, pch=NULL, bg=NULL,
                              eos="gsw",
                              mapControl=NULL,
                              profileControl=NULL,
                              QCControl=NULL,
                              summaryControl=NULL,
                              TSControl=NULL,
                              debug=0,
                              ...)
          {
              if (!requireNamespace("oce", quietly=TRUE))
                  stop("must install.packages(\"oce\") for plot() to work")
              debug <- if (debug > 2) 2 else max(0, floor(debug + 0.5))
              argoFloatsDebug(debug, "plot(x, which=\"", which, "\") {\n", sep="", unindent=1, style="bold")
              dots <- list(...)
              if (!inherits(x, "argoFloats"))
                  stop("In plot() : method is only for objects of class \"argoFloats\"", call.=FALSE)
              if (length(which) != 1)
                  stop("\"which\" must contain only one item")
              if (!geographical %in% c(0, 1, 4))
                  stop("In plot() : \"geographical\" must be 0, 1, or 4", call.=FALSE)
              istraj <- identical(x@metadata$subtype, "trajectories")
              if (which == "map") {
                  data("coastlineWorld", package="oce", envir=environment())
                  argoFloatsDebug(debug, "map plot\n", sep="")
                  if (!istraj) {
                      longitude <- x[["longitude", debug=debug]]
                      latitude <- x[["latitude", debug=debug]]
                  }
                  if (istraj) {
                      lat1 <- as.numeric(x[["latitude_min"]])
                      lat2 <- as.numeric(x[["latitude_max"]])
                      lon1 <- as.numeric(x[["longitude_min"]])
                      lon2 <- as.numeric(x[["longitude_max"]])
                      latitude <- c(lat1, lat2)
                      longitude <- c(lon1, lon2)
                  }
                  n <- x[["length"]]
                  ## Find type of each cycle, for colour-coding
                  cycleType <- rep("core", n)
                  cycleType[("849" == x@data$index$profiler_cycleType)] <- "deep"
                  cycleType[("862" == x@data$index$profiler_cycleType)] <- "deep"
                  cycleType[("864" == x@data$index$profiler_cycleType)] <- "deep"
                  cycleType[!is.na(x@data$index$parameters)] <- "Bgc"
                  colLand <- mapControl$colLand
                  if (is.null(mapControl))
                      mapControl <- list(bathymetry=bathymetry, projection=FALSE, colLand="lightgray")
                  if (!"bathymetry" %in% names(mapControl))
                      mapControl$bathymetry <- TRUE
                  if (!"projection" %in% names(mapControl))
                      mapControl$projection <- FALSE
                  if (is.logical(mapControl$projection)) {
                      mapControl$projection <- if (mapControl$projection) "+proj=moll" else "none"
                  }
                  if (substr(mapControl$projection, 1, 6) != "+proj=" && mapControl$projection != "none")
                      stop("In plot,argoFloats-method(): mapControl$projection must start with \"+proj=\"", call.=FALSE)
                  if (mapControl$projection != "none") {
                      data("coastlineWorld", package="oce", envir=environment())
                      coastlineWorld <- get("coastlineWorld")
                      oce::mapPlot(coastlineWorld, col= "lightgray", projection=mapControl$projection, drawBox=FALSE)
                      if (!istraj) {
                          oce::mapPoints(unlist(longitude), unlist(latitude),
                                         cex=if (is.null(cex)) 1 else cex,
                                         col=if (is.null(col)) "white" else col,
                                         pch=if (is.null(pch)) 21 else pch,
                                         bg=if (is.null(bg)) "red" else bg,
                                         type=if (is.null(type)) "p" else type,
                                         ...)
                      } else if (istraj) {
                          rect(lon1,lat1, lon2,lat2)
                      }
                      ## warning("In plot,argoFloats-method(): projected maps do not (yet) show bathymetry", call.=FALSE)
                      return(invisible(NULL))
                  }
                  if (is.null(xlim))
                      if (!istraj) {
                          xlim <- extendrange(longitude)
                      } else if (istraj) {
                          xlim <- extendrange(range(lon1,lon2))
                      }
                  if (is.null(ylim))
                      if (!istraj) {
                          ylim <- extendrange(latitude)
                      } else if (istraj) {
                          ylim <- extendrange(range(lat1, lat2))
                      }

                  ## Draw empty plot box, with axes, to set par("usr") for later use with bathymetry.
                  xlab <- if (is.null(xlab)) "" else xlab
                  ylab <- if (is.null(ylab)) "" else ylab
                  ## Decode bathymetry
                  if (is.logical(mapControl$bathymetry)) {
                      drawBathymetry <- mapControl$bathymetry
                      bathymetry <- list(source="auto", contour=FALSE, colormap="auto", palette=TRUE)
                  } else if (is.list(mapControl$bathymetry)) {
                      drawBathymetry <- TRUE
                      if (!("source" %in% names(mapControl$bathymetry)))
                          stop("In plot() : \"bathymetry\" is a list, it must contain \"source\", at least", call.=FALSE)
                      if ("keep" %in% names(bathymetry))
                          warning("bathymetry$keep (an old argument) is no longer used\n")
                      if (is.null(bathymetry$contour))
                          bathymetry$contour <- FALSE
                      if (is.null(bathymetry$colormap))
                          bathymetry$colormap <- "auto"
                      if (is.null(bathymetry$palette))
                          bathymetry$palette <- TRUE
                  } else {
                      stop("In plot() : \"bathymetry\" must be either a logical or a list value", call.=FALSE)
                  }
                  if (!is.logical(bathymetry$palette))
                      stop("In plot() : \"bathymetry$palette\" must be a logical value", call.=FALSE)
                  argoFloatsDebug(debug, "drawBathymetry calculated to be ", drawBathymetry, "\n", sep="")
                  asp <- 1 / cos(pi/180*mean(range(latitude, na.rm=TRUE)))
                  argoFloatsDebug(debug, "asp=", asp, "\n", sep="")
                  if (drawBathymetry) {
                      argoFloatsDebug(debug, "handling bathymetry\n", sep="")
                      if (!requireNamespace("oce", quietly=TRUE))
                          stop("must install.packages(\"oce\") to plot with bathymetry")
                      ## Handle bathymetry file downloading (or the use of a supplied value)
                      bathy <- NULL
                      if (is.character(bathymetry$source) && bathymetry$source == "auto") {
                          argoFloatsDebug(debug, "must either download bathymetry or use existing file\n", sep="")
                          if (!bathymetry$contour && bathymetry$palette) {
                              omar <- par("mar")
                              par(mar=omar + c(0, 0, 0, 2.75))
                              on.exit(par(mar=omar))
                              argoFloatsDebug(debug, "temporarily set par(mar=c(", paste(par("mar"), collapse=", "), ")) to allow for the palette\n", sep="")
                          }
                          argoFloatsDebug(debug, "using plot.window() to determine area for bathymetry download, with\n",
                                          "    extendrange(longitude)=c(", paste(extendrange(longitude), collapse=","), ")\n",
                                          "    extendrange(latitude)=c(", paste(extendrange(latitude), collapse=","), ")\n",
                                          "    xlim=c(", paste(xlim, collapse=","), ")\n",
                                          "    ylim=c(", paste(ylim, collapse=","), ")\n",
                                          "    asp=", asp, "\n", sep="")
                          plot.window(xlim=xlim, ylim=ylim,
                                      xaxs="i", yaxs="i",
                                      asp=asp,
                                      xlab=xlab, ylab=ylab)
                          usr <- par("usr")
                          argoFloatsDebug(debug, "after using plot.window(), usr=c(", paste(round(usr, 4), collapse=", "), ")\n", sep="")
                          latitudeSpan <- usr[4] - usr[3]
                          longitudeSpan <- usr[2] - usr[1]
                          Dlon <- 0.05 * longitudeSpan
                          Dlat <- 0.05 * latitudeSpan
                          argoFloatsDebug(debug, "Dlon=", Dlon, " (5% of longitude span, which is ", longitudeSpan, ")\n", sep="")
                          argoFloatsDebug(debug, "Dlat=", Dlat, " (5% of latitude span, which is ", latitudeSpan, ")\n", sep="")
                          resolution <- as.integer(round(1 + 60 * latitudeSpan / 400))
                          if (resolution < 1)
                              stop("calculated resolution (=", resolution, ") cannot be under 1 minute")
                          argoFloatsDebug(debug, "resolution=", resolution, "\n", sep="")
                          ## Notice the use of 0.1 and 1 digit in lon and lat, which means
                          ## we are enlarging the field by 10km in both lon and lat.
                          west <- round(usr[1]-Dlon, 1)
                          east <- round(usr[2]+Dlon, 1)
                          south <- round(usr[3]-Dlat, 1)
                          north <- round(usr[4]+Dlat, 1)
                          argoFloatsDebug(debug, "will get topo in domain west=", west, ", east=", east,
                              ", south=", south, ", north=", north, "\n")
                          topo <- try(oce::read.topo(oce::download.topo(west=west,
                                      east=east,
                                      south=south,
                                      north=north,
                                      resolution=resolution,
                                      debug=debug)),
                              silent=FALSE)
                          if (inherits(topo, "try-error")) {
                              warning("could not download bathymetry from NOAA server\n")
                              drawBathymetry <- FALSE
                          } else {
                              bathy <- -topo[["z"]]
                              dimBathy <- dim(bathy)
                              argoFloatsDebug(debug, "grid size", paste(dimBathy, collapse="x"), "\n")
                              bathy <- as.integer(bathy)
                              dim(bathy) <- dimBathy
                              rownames(bathy) <- topo[["longitude"]]
                              colnames(bathy) <- topo[["latitude"]]
                              class(bathy) <- "bathy"
                          }
                      } else if (inherits(bathymetry$source, "bathy")) {
                          argoFloatsDebug(debug, "using supplied bathymetry$source\n", sep="")
                          bathy <- bathymetry$source
                      } else if (inherits(bathymetry$source, "oce") && inherits(bathymetry$source, "topo")) {
                          argoFloatsDebug(debug, "using oce-style topo object (converted to NOAA bathy)\n", sep="")
                          dim <- dim(bathymetry$source[["z"]])
                          ## Note the negative sign, to get depth.
                          bathy <- matrix(-as.integer(bathymetry$source[["z"]]), nrow=dim[1], ncol=dim[2])
                          rownames(bathy) <- bathymetry$source[["longitude"]]
                          colnames(bathy) <- bathymetry$source[["latitude"]]
                          class(bathy) <- "bathy"
                      } else {
                          stop("cannot determine bathymetry data source")
                      }
                      argoFloatsDebug(debug, "drawBathymetry = ", drawBathymetry, " (after trying to get bathymetry)\n", sep="")
                      ## Handle colormap
                      if (drawBathymetry && !inherits(bathy, "try-error") && !bathymetry$contour && is.character(bathymetry$colormap) && length(bathymetry$colormap) == 1 && bathymetry$colormap == "auto") {
                          argoFloatsDebug(debug, "bathy class: \"", class(bathy), "\"\n", sep="")
                          deepest <- max(bathy, na.rm=TRUE)
                          argoFloatsDebug(debug, "bathy range: ", paste(range(bathy, na.rm=TRUE), collapse=" to "), "\n")
                          argoFloatsDebug(debug, "setting a default colormap for ", deepest, "m to 0m depth\n", sep="")
                          bathymetry$colormap <- oce::colormap(zlim=c(0, deepest),
                                                               col=function(n) rev(oce::oceColorsGebco(n)))
                      }
                      ## Handle optional drawing of the palette. Note space for one line of text is removed
                      ## from the LHS of the palette and added to the RHS.  This is because we know that
                      ## there is no axis to the left of the palette, so we do not need space for one.
                      ## We recover the stolen space by putting it back at the RHS, where it can be
                      ## useful, especially if a map plot has another plot to its right.
                      if (drawBathymetry && !inherits(bathy, "try-error") && !bathymetry$contour && bathymetry$palette) {
                          argoFloatsDebug(debug, "drawing a bathymetry palette\n")
                          omai <- par("mai") # save this, since oce::drawPalette() may change it
                          oce::drawPalette(colormap=bathymetry$colormap, cex=if (is.null(cex)) 1 else par("cex.axis"))
                          on.exit(par(mai=omai))
                      } else {
                          argoFloatsDebug(debug, "not drawing a bathymetry palette, as instructed (or failed bathymetry download)\n")
                      }
                  }
                  argoFloatsDebug(debug, "initial xlim=",
                                  "c(", paste(xlim, collapse=","), ") and ylim=",
                                  "c(", paste(ylim, collapse=","), ")\n", sep="")
                  ## Extend to world edge, if we are close and scale is large
                  if (diff(xlim) > 340) {
                      if (xlim[1] < -170) xlim[1] <- -180
                      if (xlim[2] >  170) xlim[2] <-  180
                  }
                  if (diff(ylim) > 160) {
                      if (ylim[1] < -70) ylim[1] <- -90
                      if (ylim[2] >  70) ylim[2] <-  90
                  }
                  argoFloatsDebug(debug, "after to-world-edge, xlim=",
                                  "c(", paste(xlim, collapse=","), ") and ylim=",
                                  "c(", paste(ylim, collapse=","), ")\n", sep="")
                  ## Trim to world domain
                  xlim <- ifelse(xlim < -180, -180, ifelse(xlim > 180, 180, xlim))
                  ylim <- ifelse(ylim <  -80,  -90, ifelse(ylim >  80,  90, ylim))
                  argoFloatsDebug(debug, "after domain-trim, xlim=",
                                  "c(", paste(xlim, collapse=","), ") and ylim=",
                                  "c(", paste(ylim, collapse=","), ")\n", sep="")
                  argoFloatsDebug(debug, "about to plot, with\n",
                                  "    extendrange(longitude)=c(", paste(extendrange(longitude), collapse=","), ")\n",
                                  "    extendrange(latitude)=c(", paste(extendrange(latitude), collapse=","), ")\n",
                                  "    xlim=c(", paste(xlim, collapse=","), ")\n",
                                  "    ylim=c(", paste(ylim, collapse=","), ")\n",
                                  "    asp=", asp, "\n", sep="")
                  plot(xlim, ylim,
                       xaxs="i", yaxs="i",
                       asp=asp,
                       xlab=xlab, ylab=ylab, type="n", axes=FALSE)
                  ## Draw box and axes.  Note that we cannot just use axis() because we
                  ## do not want to show e.g. lat beyond 90N.
                  usrTrimmed <- par("usr")
                  usrTrimmed[1] <- max(-180, usrTrimmed[1])
                  usrTrimmed[2] <- min(180, usrTrimmed[2])
                  usrTrimmed[3] <- max(-90, usrTrimmed[3])
                  usrTrimmed[4] <- min(90, usrTrimmed[4])
                  rect(usrTrimmed[1], usrTrimmed[3], usrTrimmed[2], usrTrimmed[4], lwd=1)
                  ## Construct axes
                  xat <- pretty(usrTrimmed[1:2])
                  if (diff(range(xat)) > 300)
                      xat <- seq(-180, 180, 45)
                  yat <- pretty(usrTrimmed[3:4])
                  if (diff(range(yat)) > 150)
                      yat <- seq(-90, 90, 45)
                  if (geographical == 0) {
                      xlabels <- xat
                      ylabels <- yat
                  } else if (geographical == 1) {
                      xlabels <- abs(xat)
                      ylabels <- abs(yat)
                  } else if (geographical == 4) {
                      xlabels <- paste(abs(xat), ifelse(xat < 0, "W", ifelse(xat > 0, "E", "")), sep="")
                      ylabels <- paste(abs(yat), ifelse(yat < 0, "S", ifelse(yat > 0, "N", "")), sep="")
                  } else {
                      stop("In plot,argoFloats-method() : programming error: \"geographical\" must be 0, 1, or 4", call.=FALSE)
                  }
                  axis(1, at=xat, labels=xlabels, pos=usrTrimmed[3])
                  axis(2, at=yat, labels=ylabels, pos=usrTrimmed[1])
                  axis(3, at=xat, labels=FALSE, pos=usrTrimmed[4])
                  axis(4, at=yat, labels=FALSE, pos=usrTrimmed[2])
                  ## argoFloatsMapAxes(geographical=geographical)
                  if (drawBathymetry) {
                      if (bathymetry$contour) {
                          argoFloatsDebug(debug, "indicating bathymetry with contours\n")
                          contour(as.numeric(rownames(bathy)),
                                  as.numeric(colnames(bathy)),
                                  bathy,
                                  levels=c(100, 200, 500, seq(1e3, 10e3, 1e3)),
                                  labels=c("100m", "200m", "500m", paste(1:10, "km", sep="")),
                                  labcex=0.9, add=TRUE)
                      } else {
                          argoFloatsDebug(debug, "indicating bathymetry with an image\n")
                          oce::imagep(as.numeric(rownames(bathy)),
                                      as.numeric(colnames(bathy)),
                                      bathy, colormap=bathymetry$colormap, add=TRUE)
                      }
                      rect(usrTrimmed[1], usrTrimmed[3], usrTrimmed[2], usrTrimmed[4], lwd=1)
                  }
                  if (is.null(col)) {
                      col <- rep("", n)
                      col[cycleType == "core"] <- colDefaults$core
                      col[cycleType == "Bgc"] <- colDefaults$bgc
                      col[cycleType == "deep"] <- colDefaults$deep
                  }
                  ## Select coastline.  Unlike in oce::plot,coastline-method, we base our choice
                  ## on just the distance spanned in the north-south direction.
                  ocedataIsInstalled <- requireNamespace("ocedata", quietly=TRUE)
                  if (ocedataIsInstalled) {
                      usr <- par("usr")
                      mapSpan <- diff(range(latitude, na.rm=TRUE)) * 111 # km
                      ##? l <- oce::geodDist(pinlon(usr[1]), pinlat(usr[3]), pinlon(usr[1]), pinlat(usr[4]))
                      ##? r <- oce::geodDist(pinlon(usr[2]), pinlat(usr[3]), pinlon(usr[2]), pinlat(usr[4]))
                      ##? b <- oce::geodDist(pinlon(usr[1]), pinlat(usr[1]), pinlon(usr[2]), pinlat(usr[1]))
                      ##? t <- oce::geodDist(pinlon(usr[1]), pinlat(usr[4]), pinlon(usr[2]), pinlat(usr[4]))
                      ##? mapSpan <- max(l, r, b, t) # largest length [km]
                      C <- 2 * 3.14 * 6.4e3 # circumferance of earth [km]
                      argoFloatsDebug(debug, "mapSpan=", mapSpan, ", C=", C, "\n")
                      if (mapSpan < 500) {
                          argoFloatsDebug(debug, "using coastlineWorldFine from ocedata package\n")
                          data("coastlineWorldFine", package="ocedata", envir=environment())
                          coastlineWorldFine <- get("coastlineWorldFine")
                          polygon(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]], col= if (is.null(colLand)) "lightgray" else colLand)
                      } else if (mapSpan < C / 4) {
                          argoFloatsDebug(debug, "using coastlineWorldMedium from ocedata package\n")
                          data("coastlineWorldMedium", package="ocedata", envir=environment())
                          coastlineWorldMedium <- get("coastlineWorldMedium")
                          polygon(coastlineWorldMedium[["longitude"]], coastlineWorldMedium[["latitude"]], col= if (is.null(colLand)) "lightgray" else colLand)
                      } else {
                          argoFloatsDebug(debug, "using coastlineWorld from oce package, since the span is large\n")
                          data("coastlineWorld", package="oce", envir=environment())
                          coastlineWorld <- get("coastlineWorld")
                          polygon(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]], col= if (is.null(colLand)) "lightgray" else colLand)
                      }
                      if (is.null(pch))
                          pch <- 21
                      type <- if (is.null(type)) "p" else type
                      if (length(pch) == 1 && pch == 21 && !istraj) {
                          if (type != "l") {
                              points(unlist(longitude), unlist(latitude),
                                     cex=if (is.null(cex)) 1 else cex,
                                     pch=pch,
                                     bg=col,
                                     type= type,
                                     ...)
                          } else if (type == "l") {
                              index1 <- subset(x, ID=unique(x[["ID"]][1]), silent=TRUE)
                              longitude <- index1[["longitude"]]
                              latitude <- index1[["latitude"]]
                              points(unlist(longitude), unlist(latitude),
                                     cex=if (is.null(cex)) 1 else cex,
                                     pch=pch,
                                     bg=col,
                                     type= "l",
                                     ...)
                              for (i in  unique(x[["ID"]])[2:length(unique(x[["ID"]]))]) {
                                  index2 <- subset(x, ID=i, silent=TRUE)
                                  points(unlist(index2[["longitude"]]), unlist(index2[["latitude"]]),
                                         cex=if (is.null(cex)) 1 else cex,
                                         pch=pch,
                                         bg=col,
                                         type= "l",
                                         ...)
                              }
                          }
                      } else if (!istraj) {
                          points(unlist(longitude), unlist(latitude),
                                 cex=if (is.null(cex)) 1 else cex,
                                 pch=pch,
                                 col=col,
                                 type=if (is.null(type)) "p" else type,
                                 ...)
                      } else if (istraj) {
                          rect(lon1,lat1, lon2,lat2)
                      }
                  } else {
                      argoFloatsDebug(debug, "using coastlineWorld from oce package, since the ocedata package is not installed\n")
                      data("coastlineWorld", package="ocedata", envir=environment())
                      coastlineWorld <- get("coastlineWorld")
                      polygon(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]], col=col)
                  }
              } else if (which == "summary" && !istraj) {
                  argoFloatsDebug(debug, "summary plot\n", sep="")
                  if (x[["length"]] < 2)
                      stop("In plot,argoFloats-method() : cannot draw a summary plot with only one float cycle", call.=FALSE)
                  if (!all(names(dots) %in% c("summaryControl")))
                      stop(names(dots)," is not a valid argument for this plot type. Try summaryControl instead.")
                  if (is.null(summaryControl))
                      summaryControl <- list(items=c("dataStateIndicator", "longitude", "latitude", "length", "deepest"))
                  if (!"items" %in% names(summaryControl))
                      stop("summaryControl must be a list containing a character vector named \"items\"")

                  if (!(summaryControl$items) %in% c("dataStateIndicator", "longitude", "latitude", "length", "deepest"))
                      stop("The names of the items must be dataStateIndicator, longitude, latitude, length, or deepest, not ", paste(summaryControl$items, collapse=" "))
                  items <- summaryControl$items
                  nitems <- length(items)
                  if (nitems) {
                      omfrow <- par("mfrow")
                      omar <- par("mar")
                      omgp <- par("mgp")
                      par(mar=c(3,3,2,1.5), mgp=c(1.75, 0.5, 0))
                      if (nitems < 3)
                          par(mfrow=c(nitems, 1))
                      else if (nitems == 4)
                          par(mfrow=c(2, 2))
                      else
                          par(mfrow=c(3, 2))
                      on.exit(par(mfrow=omfrow, mar=omar, mgp=omgp))
                      time <- as.POSIXct(unlist(x[["time"]]), origin="1970-01-01", tz="UTC")
                      o <- order(time)
                      for (iitem in seq_len(nitems)) {
                          argoFloatsDebug(debug, "items[", iitem, "]=\"", items[iitem], "\"\n", sep="")
                          if (items[iitem] == "dataStateIndicator") {
                              y <- unlist(x[["dataStateIndicator"]])
                              argoFloatsDebug(debug, oce::vectorShow(y))
                              if (length(y)) {
                                  u <- sort(unique(y))
                                  yy <- seq_along(u)
                                  oce::oce.plot.ts(range(time), range(yy), ylab="Data State Ind.",
                                      drawTimeRange=FALSE, type="n", axes=FALSE,
                                      xaxs="i", ylim=range(0.8, (max(yy)+0.2)),
                                      mar=par("mar"), mgp=par("mgp"))
                                  abline(h=seq_along(u), col="gray")
                                  points(time[o], factor(y)[o])
                                  oce::oce.axis.POSIXct(side=1, drawTimeRange=FALSE)
                                  box()
                                  axis(side=2, at=yy, labels=u)
                              }
                          } else if (items[iitem] == "length") {
                              y <- sapply(x[["argos"]], function(a) length(a[["pressure"]]))
                              oce::oce.plot.ts(time[o], y[o], ylab="Length", drawTimeRange=FALSE,
                                  type="p", xaxs="i",
                                  mar=par("mar"), mgp=par("mgp"))
                          } else if (items[iitem] == "longitude") {
                              y <- sapply(x[["longitude"]], function(a) a[1])
                              oce::oce.plot.ts(time[o], y[o], ylab="Longitude", drawTimeRange=FALSE,
                                  type="p", xaxs="i",
                                  mar=par("mar"), mgp=par("mgp"))
                          } else if (items[iitem] == "latitude") {
                              y <- sapply(x[["latitude"]], function(a) a[1])
                              oce::oce.plot.ts(time[o], y[o], ylab="Latitude", drawTimeRange=FALSE,
                                  type="p", xaxs="i",
                                  mar=par("mar"), mgp=par("mgp"))
                          } else if (items[iitem] == "deepest") {
                              y <- sapply(x[["argos"]], function(a) max(a[["pressure"]], na.rm=TRUE))
                              oce::oce.plot.ts(time[o], y[o], ylab="Max Pres.", drawTimeRange=FALSE,
                                  type="p", xaxs="i",
                                  mar=par("mar"), mgp=par("mgp"))
                          }
                          if (iitem == 1) {
                              if (1 == length(unique(x[["ID"]])))
                                  axis(side=3, at=time[o], labels=x[["cycle"]][o],
                                       cex.axis=max(0.75, 0.75*par("cex")))
                          }
                      }
                  }
              } else if (which == "summary" && istraj) {
                      stop("In plot,argoFloats-method(): cannot draw summary plot for subtype = trajectories", call.=FALSE)

              } else if (which == "TS") {
                  argoFloatsDebug(debug, "TS plot\n", sep="")
                  if ((x[["type"]] != "argos"))
                      stop("In plot,argoFloats-method(): x must have been created by readProfiles()", call.=FALSE)
                  if (!(eos %in% c("gsw", "unesco")))
                      stop("In plot,argoFloats-method(): eos must be \"gsw\" or \"unesco\", not \"", eos, "\"", call.=FALSE)
                  salinity <- unlist(x[["salinity", debug=debug]])
                  temperature <- unlist(x[["temperature", debug=debug]])
                  pressure <- unlist(x[["pressure", debug=debug]])
                  ## Use byLevel to repeat the latitude and longitude values across
                  ## the depths in each profile, so that the resultant vector
                  ## will match the salinity, temperature and pressure vectors.
                  latitude <- unlist(x[["latitude", "byLevel", debug=debug]])
                  longitude <- unlist(x[["longitude", "byLevel", debug=debug]])
                  # Interpolate across NA longitudes (required for traj data, to get TS plot)
                  n <- length(longitude)
                  if (any(is.na(longitude)))
                      longitude <- approx(1:n, longitude, 1:n)$y
                  if (any(is.na(latitude)))
                      latitude <- approx(1:n, latitude, 1:n)$y
                  ctd <- oce::as.ctd(salinity=salinity,
                                     temperature=temperature,
                                     pressure=pressure,
                                     latitude=latitude,
                                     longitude=longitude)
                  if (is.null(TSControl))
                      TSControl <- list(colByCycle=NULL)
                  if (is.null(TSControl$colByCycle)) {
                      if (is.null(col)) {
                          if (which == "TS") {
                              col <- "flags"
                          } else {
                              col <- rgb(0, 0, 1, 0.5)
                          }
                      }
                  } else {
                      ## Ignore "col" if TSControl contains "colByCycle"
                      colByCycle <- TSControl$colByCycle
                      cycle <- unlist(x[["cycle", debug=debug]])
                      lengths <- sapply(x[["argos"]], function(cc) length(cc[["pressure"]]))
                      ## Increase the col length, so e.g. TSControl=list(colByCycle=1:2) will alternate colours
                      colByCycle <- rep(colByCycle, length.out=length(cycle))
                      col <- unlist(lapply(seq_along(cycle), function(i) rep(colByCycle[i], lengths[i])))
                  }
                  if (is.null(cex)) {
                      cex <- par("cex")
                      argoFloatsDebug(debug, "TS plot defaulting to cex=", cex, "\n")
                  }
                  if (is.null(pch)) {
                      pch <- 20
                      argoFloatsDebug(debug, "TS plot defaulting to pch=", pch, "\n")
                  }
                  if (col[1] == "flags") {
                      argoFloatsDebug(debug, "col is \"flags\"\n")
                      salinityFlag <- unlist(x[["salinityFlag"]])
                      temperatureFlag <- unlist(x[["temperatureFlag"]])
                      goodT <- temperatureFlag %in% c(1, 2, 5, 8)
                      goodS <- salinityFlag %in% c(1, 2, 5, 8)
                      good <- goodS & goodT
                      okT <- temperatureFlag %in% c(0)
                      okS <- salinityFlag %in% c(0)
                      ok <- okS & okT
                      col <- ifelse(good, "black", ifelse(ok, "gray", "red"))
                      if (pch == 21)
                          bg <- ifelse(good, "black", ifelse(ok, "gray", "red"))
                  }
                  oce::plotTS(ctd,
                              cex=cex,
                              bg=bg,
                              col=col,
                              pch=pch,
                              mar=par("mar"), mgp=par("mgp"), eos=eos,
                              type=if (is.null(type)) "p" else type, ...)
                  #par(mar=omar, mgp=omgp)
              } else if (which == "QC" && !istraj) {
                  if (x[["type"]] != "argos")
                      stop("In plot,argoFloats-method(): The type of x must be \"argos\"", call.=FALSE)
                  IDs <- x[["ID"]]
                  nID <- length(unique(IDs))
                  if (nID != 1)
                      stop("In plot,argoFloats-method(): It is only possible to plot a QC of a single ID", call.=FALSE)
                  knownParameters <- names(x[[1]]@metadata$flags) # FIXME: is it possible that later cycles have different flags?
                  if (!all(names(dots) %in% c("QCControl")))
                      stop(names(dots)," is not a valid argument for this plot type. Try QCControl instead.")
                  if (is.null(QCControl)) {
                      QCControl <- list(parameter="temperature")
                  }
                  if (!is.list(QCControl))
                      stop("In plot,argoFloats-method(): QCControl must be a list")
                  if (!all(names(QCControl) %in% c("parameter", "dataStateIndicator")))
                      stop("QCControl must contain only elements named \"parameter\" and \"dataStateIndicator\", but its elements are named \"", paste(names(QCControl), collapse="\", \""), "\"")
                  if (!"parameter" %in% names(QCControl))
                      QCControl$parameter <- "temperature"
                  if (!"dataStateIndicator" %in% names(QCControl))
                      QCControl$dataStateIndicator <- FALSE
                  if (!is.logical(QCControl$dataStateIndicator))
                      stop("QCControl element 'dataStateIndicator' must be logical")
                  if (length(QCControl) != 2)
                      stop("In plot,argoFloats-method(): QCControl must contain only elements named \"parameter\" and \"dataStateIndicator\"")
                  if (!(QCControl$parameter %in% knownParameters))
                      stop("In plot,argoFloats-method(): QCControl$parameter '", QCControl$parameter, "' not found. Try one of: \"", paste(sort(knownParameters), collapse="\", \""), "\"", call.=FALSE)
                  qf <- function(x) {
                      # qf returns 100 if data are all "good" = 1 or "probably good" = 2 or "changed" = 5 or "estimated" = 8
                      flag <- x[[paste0(QCControl$parameter, "Flag")]]
                      100 * sum(1 == flag | 2 == flag | 5 == flag | 8 == flag, na.rm=TRUE) / length(flag)
                  }
                  meanf <- function(x)
                      mean(x[[QCControl$parameter]], na.rm=TRUE)
                  time <- oce::numberAsPOSIXct(unlist(lapply(x[["argos"]], function(x) x[["time"]])))
                  q <- unlist(lapply(x[["argos"]], qf))
                  m <- unlist(lapply(x[["argos"]], meanf))
                  omfrow <- par("mfrow")
                  omar <- par("mar")
                  par(mar=c(3, 3, 2, 1))
                  par(mfrow=c(if (QCControl$dataStateIndicator) 3 else 2, 1))
                  on.exit(par(mfrow=omfrow, mar=omar))
                  if (any(is.finite(q))) {
                      o <- order(time) # cycles are not time-ordered in index files
                      oce::oce.plot.ts(time[o], q[o], ylab=paste(QCControl$parameter, "% Good"), drawTimeRange=FALSE, type="l", mar=par("mar"), xaxs="i", cex.lab=0.7, cex.axis=0.7)
                      points(time[o], q[o], col=ifelse(q[o] < 50, "red", "black"), pch=20, cex=1)
                      abline(h=50, col="red", lty="dashed")
                      if (1 == length(unique(x[["ID"]])))
                          axis(side=3, at=time[o], labels=x[["cycle"]][o], cex.axis=0.7, cex.lab=0.7)
                      if (QCControl$dataStateIndicator) {
                          y <- unlist(x[["dataStateIndicator"]])
                          if (length(y)) {
                              u <- sort(unique(y))
                              yy <- seq_along(u)
                              oce::oce.plot.ts(range(time), range(yy), ylab="Data State Ind.",
                                               drawTimeRange=FALSE, type="n", axes=FALSE,
                                               mar=par("mar"), mgp=par("mgp"),
                                               xaxs="i",  ylim=range(0.8, (max(yy)+0.2)))
                              abline(h=seq_along(u), col="gray")
                              points(time[o], factor(y)[o])
                              oce::oce.axis.POSIXct(side=1, drawTimeRange=FALSE)
                              box()
                              axis(side=2, at=yy, labels=u)
                          } else {
                              plot(0:1, 0:1, xlab="", ylab='', type="n", axes=FALSE)
                              box()
                              text(0.5, 0.5, "x lacks dataStateIndicator")
                          }
                      }
                      oce::oce.plot.ts(time[o], m[o], ylab=paste(QCControl$parameter, "Mean"), drawTimeRange=FALSE, type="l",
                          mar=par("mar"), cex.lab=0.7, cex.axis=0.7)
                      points(time[o], m[o], col=ifelse(q[o] < 50, "red", "black"), pch=20, cex=1)
                  } else {
                      plot(0:1, 0:1, xlab="", ylab='', type="n", axes=FALSE)
                      box()
                      text(0, 0.5, paste(' No', QCControl$parameter, "flags available"), pos=4)
                      plot(0:1, 0:1, xlab="", ylab='', type="n", axes=FALSE)
                      box()
                      text(0, 0.5, paste(' No', QCControl$parameter, "flags available"), pos=4)
                  }
              } else if (which == "QC" && istraj) {
                      stop("In plot,argoFloats-method(): cannot draw QC plot for subtype = trajectories", call.=FALSE)

              } else if (which == "profile") {
                  if (x[["type"]] != "argos")
                      stop("In plot,argoFloats-method(): The type of x must be \"argos\"", call.=FALSE)
                  if (is.null(type))
                      type <- "p"
                  if (is.null(profileControl)) {
                      if ("parameter" %in% names(dots)) {
                          warning("accepting \"parameter\" as a separate argument, until 2020-dec-01.  After that, you must use e.g. profileControl=list(parameter=", dots$parameter, ")")
                          profileControl <- list(parameter=dots$parameter)
                      } else {
                          profileControl <- list(parameter="temperature")
                      }
                      profileControl$ytype <- "pressure"
                      profileControl$connect <- FALSE
                  }
                  if (!is.list(profileControl))
                      stop("In plot,argoFloats-method(): profileControl must be a list")
                  if (!"ytype" %in% names(profileControl))
                      profileControl$ytype <- "pressure"
                  if (!"parameter" %in% names(profileControl))
                      profileControl$parameter <- "temperature"
                  if (!"connect" %in% names(profileControl))
                      profileControl$connect <- TRUE
                  if (length(profileControl) != 3)
                      stop("In plot,argoFloats-method(): profileControl must contain only three elements, \"parameter\",  \"ytype\" and \"connect\".")
                  if (!profileControl$ytype %in% c("pressure", "sigma0"))
                      stop("In plot,argoFloats-method(): profileControl$ytype must be \"pressure\" or \"sigma0\", not \"", profileControl$ytype, "\"")
                  N <- length(x[["argos"]])
                  ## The known parameter names include not just the things stored in the
                  ## data slot (of *any* of the profiles), but also some computable things. We
                  ## will always have longitude and latitude, so there's no need to check for them.
                  ## A core profile will always get the computable things, but bgc profiles
                  ## may lack e.g. salinity and temperature, so all we need to check for is
                  ## salinity, temperature, and pressure.
                  knownParameters <- unique(unlist(lapply(seq_len(N), function(i) names(x[[i]][["data"]]))))
                  if (all(c("salinity", "temperature", "pressure") %in% knownParameters)) {
                      knownParameters <- c(knownParameters,
                                           "SA", "CT", "sigma0", "sigma1", "sigma2", "sigma3", "sigma4", "sigmaTheta",
                                           "theta",
                                           "spice",
                                           "N2")
                  }
                  argoFloatsDebug(debug, "knownParameters: \"", paste(sort(knownParameters), collapse="\", \""), "\".\n", sep="")
                  if (!(profileControl$parameter %in% knownParameters))
                      stop("In plot,argoFloats-method(): profileControl$parameter=\"", profileControl$parameter, "\" is not in the dataset, or calculable from that dataset; try one of the following: \"", paste(sort(knownParameters), collapse='", "'), "\".", call.=FALSE)
                  y <- lapply(1:N, function(i) x[[i]][[profileControl$ytype]])
                  variable <- lapply(1:N, function(i) x[[i]][[profileControl$parameter]])
                  nn <- unlist(lapply(1:N, function(i) prod(dim(x[[i]][[profileControl$parameter]]))))
                  Y <- NULL
                  VARIABLE <- NULL
                  argoFloatsDebug(debug, "number of profiles: ", N, "\n")
                  for (i in seq_len(N)) {
                      if (nn[i] > 0) {
                          Y <- c(Y, NA, y[[i]])
                          VARIABLE <- c(VARIABLE, NA, variable[[i]])
                      }
                  }
                  if (type %in% c("l", "o", "b") && profileControl$connect) {
                      ok <- is.finite(VARIABLE) & is.finite(Y)
                      VARIABLE <- VARIABLE[ok]
                      Y <- Y[ok]
                  }
                  if (is.null(cex)) {
                      cex <- par("cex")
                      argoFloatsDebug(debug, "Profile plot defaulting to cex=", cex, "\n")
                  }
                  if (is.null(pch)) {
                      pch <- 20
                      argoFloatsDebug(debug, "Profile plot defaulting to pch=", pch, "\n")
                  }
                  plot(VARIABLE, Y,
                       xlim=xlim,
                       ylim=if (is.null(ylim)) rev(range(Y, na.rm=TRUE)) else ylim,
                       axes=FALSE,
                       ylab="", xlab="", # draw axes later, in oceanographic 'profile' locations
                       cex=cex,
                       type=if(is.null(type)) "l" else type,
                       col=if (is.null(col)) par("col") else col,
                       pch=pch,
                       ...)
                  box()
                  axis(2)
                  axis(3)
                  ylab <- oce::resizableLabel(if (profileControl$ytype == "pressure") "p" else "sigma0", axis="y")
                  mtext(ylab, side=2, line=par("mgp")[1], cex=par("cex"))
                  # Rename for oce::resizableLabel()
                  if (profileControl$parameter == "salinity")
                      profileControl$parameter <- "S"
                  xlab <- oce::resizableLabel(if (profileControl$parameter == "oxygen") "oxygen umol/kg" else profileControl$parameter, axis="x")
                  mtext(xlab, side=3, line=par("mgp")[1], cex=par("cex"))
              } else {
                  stop("In plot,argoFloats-method():cannot handle which=\"", which, "\"; see ?\"plot,argoFloats-method\"", call.=FALSE)
              }
              argoFloatsDebug(debug, "} # plot()\n", sep="", unindent=1, style="bold")
              invisible(NULL)
          }
)
