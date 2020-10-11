## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

## Utility functions to trim lat and lon.
pinlat <- function(lat)
    ifelse(lat < -90, -90, ifelse(90 < lat, 90, lat))
pinlon <- function(lon)
    ifelse(lon < -180, -180, ifelse(180 < lon, 180, lon))
pinusr <- function(usr)
    c(pinlon(usr[1]), pinlon(usr[2]), pinlat(usr[3]), pinlat(usr[4]))

## issue259 argoFloatsMapAxes <- function(axes=TRUE, box=TRUE, geographical=0)
## issue259 {
## issue259     ## Low-level axis plot, which limits axes to -180,180 and -90,90.
## issue259     usr <- pinusr(par("usr"))
## issue259     xat <- pretty(usr[1:2], 10)
## issue259     xat <- xat[usr[1] < xat & xat < usr[2]]
## issue259     yat <- pretty(usr[3:4], 10)
## issue259     yat <- yat[usr[3] < yat & yat < usr[4]]
## issue259     if (geographical == 0) {
## issue259         xlabels <- xat
## issue259         ylabels <- yat
## issue259     } else if (geographical == 1) {
## issue259         xlabels <- abs(xat)
## issue259         ylabels <- abs(yat)
## issue259     } else if (geographical == 4) {
## issue259         xlabels <- paste(abs(xat), ifelse(xat < 0, "W", ifelse(xat > 0, "E", "")), sep="")
## issue259         ylabels <- paste(abs(yat), ifelse(yat < 0, "S", ifelse(yat > 0, "N", "")), sep="")
## issue259     } else {
## issue259         stop("In plot() : programming error: \"geographical\" must be 0, 1, or 4", call.=FALSE)
## issue259     }
## issue259     if (axes) {
## issue259         axis(1, pos=pinlat(usr[3]), at=xat, labels=xlabels, lwd=1)
## issue259         axis(2, pos=pinlon(usr[1]), at=yat, labels=ylabels, lwd=1)
## issue259     }
## issue259     axis(3, labels=FALSE, lwd=1)
## issue259     axis(4, labels=FALSE, lwd=1)
## issue259     if (box) {
## issue259         rect(pinlon(usr[1]), pinlat(usr[3]), pinlon(usr[2]), pinlat(usr[4]), lwd=1)
## issue259     }
## issue259  }

#' Plot an argoFloats object
#'
#' The action depends on the `type` of the object, and
#' this is set up by the function that created the object;
#' see \dQuote{Details}. These are basic plot styles, with
#' somewhat limited scope for customization. Since the data with
#' [argoFloats-class] objects are easy to extract, users should
#' not find it difficult to create their own plots to meet a
#' particular aesthetic; Example 5C provides an example for
#' maps.
#'
#' The various plot types are as follows.
#'
#' * For `which="map"`, a map of profile locations is created. This
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
#'     1. `FALSE`, meaning not to draw bathymetry;
#'     2. `TRUE` (the default), meaning to draw bathymetry using
#'        data downloaded with [marmap::getNOAA.bathy()], as in Example 4;
#'     3. A list with items controlling both the bathymetry data and its
#'        representation in the plot, as in Example 5.  Those items are:
#'
#'         1. `source`, a mandatory value that is one of
#'            (a) the string `"auto"` (the default) to use
#'            [marmap::getNOAA.bathy()] to download the data,
#'            (b) a value returned by [marmap::getNOAA.bathy()], or
#'            (c) a value returned by [oce::read.topo()].
#'         2. `keep`, an optional logical value (with `TRUE` as the default) that is passed to
#'            [marmap::getNOAA.bathy()] to indicate whether to keep a local file of bathymetry,
#'            as a way to avoid intermittent problems with the NOAA server;
#'         3. `contour`, an optional logical value (with `FALSE` as the default) indicating
#'            (as in Examples 5A and 5B) whether to represent bathymetry with contours
#'            (with depths of 100m, 200m, 500m shown, along with 1km, 2km up to 10km),
#'            as opposed to an image;
#'         4. `colormap`, ignored if `contour` is `TRUE`,
#'            an optional value that is either the string `"auto"` (the default)
#'            for a form of GEBCO colors (as in Example 5C) computed with [oce::oceColorsGebco()], or a value
#'            computed with [oce::colormap()] applied to the bathymetry data; and
#'         5. `palette`, ignored if `contour` is `TRUE`,
#'            an optional logical value (with `TRUE` as the default)
#'            indicating (again, as in Example 5C) whether to draw a depth-color palette to the right of the plot.
#'
#' * For `which="profile"`, a profile plot is created, showing the variation of some quantity
#' with pressure.  This is analgous to [oce::plotProfile()] in the \CRANpkg{oce} package.
#'
#' * For `which="QC"`, two time-series panels are shown, with
#' time being that recorded in the individual profile in the dataset.
#' An additional argument named `parameter` must be given, to name the
#' quantity of interest.  The function only works if `x` is an
#' [`argoFloats-class`] object created with [readProfiles()].
#' The top panel shows the percent of data flagged with codes
#' 1 (meaning good data), 2 (probably good), 5 (changed)
#' or 8 (estimated).  Thus, low values on the top panel reveal
#' profiles that are questionable. Note that if all of data at a given time
#' have flag 0, meaning not assessed, then a quality of 0 is plotted at that
#' time. The bottom panel shows the mean value of the parameter in question
#' regardless of the flag value. See Example 7.
#'
#' * For `which="TS"`,  an overall TS plot is created.  This only works if `x`
#' is an [argoFloats-class] object of type `"argos"`, i.e. if it was
#' created by [readProfiles()]. The scales for the plot
#' can be altered by putting `Slim` and `Tlim` arguments in the `...` list; see
#' the documentation for [oce::plotTS()] for other arguments that can be
#' provided. This plot has a default color code to represent bad vs good data.
#' This scheme comes from sections 3.2.1 and 3.2.2 of Carval et al. (2019), in which
#' data are considered bad if flagged 3, 4, 6, or 7, and good
#' if flagged 1, 2, 5, or 8; good values are plotted with black symbols,
#' and bad ones are plotted with red symbols.
#'
#' @param x an [`argoFloats-class`] object.
#'
#' @param which a character value indicating the type of plot. The possible
#' choices are `"map"`, `"profile"`, `"QC"` and `"TS"`; see \dQuote{Details}.
#'
#' @param bathymetry an argument used only if `which="map"`, to control
#' whether (and how) to indicate water depth; see \dQuote{Details}.
#'
#' @param geographical flag indicating the style of axes
#' for the `which="map"` case.  With
#' `geographical=0` (which is the default),
#' the axis ticks are labelled with signed longitudes and latitudes, measured in
#' degrees. The signs are dropped with `geographical=1`.
#' In the `geographical=4` case, signs are also dropped, but hemispheres
#' are indicated by writing `S`, `N`, `W` or `E` after axis tick labels, except
#' at the equator and prime meridian.
#' Note that this scheme mimics that
#' used by [oce::plot,coastline-method()], although the
#' latter also takes values 2 and 3, which cause a display of angles in degrees,
#' minutes and seconds, which seldom makes sense for large-scale argo maps.
#'
#' @param xlim,ylim numerical values, each a two-element vector, that
#' set the `x` and `y` limits of plot axes, as for [plot.default()] and other conventional
#' plotting functions.
#'
#' @param xlab a character value indicating the name for the horizontal axis, or
#' `NULL`, which indicates that this function should choose an appropriate name
#' depending on the value of `which`. Note that `xlab` is not obeyed if
#' `which="TS"`, because altering that label can be confusing to the user.
#'
#' @param ylab as `xlab`, but for the vertical axis.
#'
#' @param type a character value that controls line type, as for [par()].  If
#' not specified, a choice is made automatically based on the value of `which`,
#' e.g. if `which` is `"map"`, points will be plotted (as if `type="p"` had been
#' given), but if `which` is `"profile"` then lines will be plotted (as if
#' `type="l"` had been given).
#'
#' @param cex a character expansion factor for plot symbols, or `NULL`, to get an
#' value that depends on the value of `which`.
#'
#' @param col the colour to be used for plot symbols, or `NULL`, to get an value
#' that depends on the value of `which` (see \dQuote{Details}).  If `which="TS"`, then the
#' `TScontrol` argument takes precedence over `col`.
#'
#' @param bg the colour to be used for plot symbol interior, for `pch`
#' values that distinguish between the interior of the symbol and the
#' border, e.g. for `pch=21`.
#'
#' @param pch an integer or code indicating the type of plot symbol, or `NULL`,
#' to get a value that depends on the value of `which`.
#' (See [par()] for more on specifying `pch`.)
#'
#' @param mar either a four-element vector giving the margins to be used for
#' the plot (see [par()] for details), or `NULL`, which means to use
#' [par]`("mar")`.
#'
#' @param mgp either a three-element vector giving the geometry for
#' axis labels (see [par()] for details), or `NULL`, which means to use
#' [par]`("mgp")`.
#'
#' @param eos a character value indicating the equation of state to use
#' if `which="TS"`.  This must be `"gsw"` (the default) or `"unesco"`;
#' see [oce::plotTS()].
#'
#' @param TScontrol a list that permits particular control of the `which="TS"`
#' case. It is ignored for the other cases.  If `TScontrol` contains a
#' vector element named `colByCycle`, then the `col` argument will be ignored,
#' and instead individual cycles will be coloured as dictated by successive
#' elements in `colByCycle`.  Note that `colByCycle` will be repeated to
#' match the number of cycles, so e.g. using `TScontrol=list(colByCycle=1:2)`
#' will colour alternate cycles with black and red.
#'
#' @param debug an integer specifying the level of debugging.
#'
#' @param \dots extra arguments passed to the plot calls that are made
#' within this function.
#'
#' @examples
#' # Example 1: map profiles in index, highlighting a neighborhood of 30
#' library(argoFloats)
#' data(index)
#' plot(index, bathymetry=FALSE)
#' lon <- index[["longitude"]]
#' lat <- index[["latitude"]]
#' dist <- oce::geodDist(lon, lat, -77.06, 26.54)
#' o <- order(dist)
#' index30 <- subset(index, o[1:30])
#' points(index30[["longitude"]], index30[["latitude"]], pch=20, col="blue")
#'
#' # Example 3: TS of first 10 profiles
#' # (Slow, so not run by default.)
#'\dontrun{
#' index10 <- subset(index, 1:10)
#' profiles10 <- getProfiles(index10, destdir="~/data/argo")
#' argos10 <- readProfiles(profiles10)
#' plot(argos10, which="TS")}
#'
#' # Example 4: map with bathymetry
#' # (Slow, so not run by default.)
#'\dontrun{
#' par(mar=c(3, 3, 1, 1))
#' plot(index, bathymetry=TRUE)}
#'
#' # Example 5: map with fine-grained bathymetry control
#' # (Slow, so not run by default.)
#'\dontrun{
#' par(mar=c(2, 2, 1, 1))
#' bathy <- marmap::getNOAA.bathy(-82, -71, 23, 30, 2, keep=TRUE)
#'
#' # Example 5A. Simple contour version, using marmap::getNOAA.bathy().
#' plot(index, bathymetry=list(source=bathy, contour=TRUE))
#'
#' # Example 5B. Simple contour version, using oce::read.topo().
#' data(topoWorld) # very coarse, read by oce::read.topo()
#' plot(index, bathymetry=list(source=topoWorld, contour=TRUE))
#'
#' # Example 5C. Simple colour version.
#' cm <- oce::colormap(zlim=c(0, -min(bathy)), col=function(...) rev(oce::oceColorsGebco(...)))
#' plot(index, bathymetry=list(source=bathy, keep=TRUE, colormap=cm, palette=TRUE))
#'
#' # Example 5D. Customized map, sidestepping this function.
#' lon <- as.numeric(rownames(bathy))
#' lat <- as.numeric(colnames(bathy))
#' depth <- -bathy # convert from elevation to depth
#' asp <- 1/cos(pi/180*mean(lat))
#' # Limit plot region to float region.
#' xlim <- range(index[["longitude"]])
#' ylim <- range(index[["latitude"]])
#' # Colourize 1km, 2km, etc, isobaths.
#' contour(x=lon, y=lat, z=depth, xlab="", ylab="",
#'         xlim=xlim, ylim=ylim, asp=asp,
#'         col=1:6, lwd=3, levels=1000*1:6, drawlabels=FALSE)
#' # Show land
#' data(coastlineWorldFine, package="ocedata")
#' polygon(coastlineWorldFine[["longitude"]],
#'         coastlineWorldFine[["latitude"]], col="tan")
#' # Indicate float positions.
#' points(index[["longitude"]], index[["latitude"]], pch=20)}
#'
#' # Example 6: TS plot for a particular argo
#' library(argoFloats)
#' a <- readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats"))
#' plot(a, which="TS")
#'
#' # Example 7: Temperature QC plot for a float in the Arabian Sea
#' \dontrun{
#' library(argoFloats)
#' ais <- getIndex(filename="synthetic", age=0)
#' sub <- subset(ais, id='2902123')
#' lonRect <- c(56, 66)
#' latRect <- c(11,12)
#' s <- subset(sub, rectangle=list(longitude=lonRect, latitude=latRect))
#' profiles <- getProfiles(s)
#' argos <- readProfiles(profiles)
#' plot(argos, which="QC", parameter="temperature")}
#'
#' # Example 8: Temperature profile of the 131st cycle of float with id 2902204
#' library(argoFloats)
#' a <- readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats"))
#' par(mgp=c(2, 0.7, 0))                  # mimic the oce::plotProfile() default
#' par(mar=c(1,3.5,3.5,2))                # mimic the oce::plotProfile() default
#' plot(a, which="profile", parameter="temperature")
#'
#' @references
#' 1. Carval, Thierry, Bob Keeley, Yasushi Takatsuki, Takashi Yoshida, Stephen Loch Loch,
#' Claudia Schmid, and Roger Goldsmith. Argo User’s Manual V3.3. Ifremer, 2019.
#' \url{https://doi.org/10.13155/29825}.
#'
#' @importFrom graphics abline axis box contour par plot.window points polygon rect text
#' @importFrom grDevices extendrange gray rgb
#' @importFrom utils data
## @importFrom oce as.ctd colormap drawPalette imagep oceColorsGebco oce.plot.ts plotTS
## @importFrom marmap getNOAA.bathy
#' @export
#' @aliases plot,argoFloats-method
#' @author Dan Kelley and Jaimie Harbin
setMethod(f="plot",
          signature=signature("argoFloats"),
          definition=function(x,
                              which="map",
                              bathymetry=TRUE,
                              geographical=0,
                              xlim=NULL, ylim=NULL,
                              xlab=NULL, ylab=NULL,
                              type=NULL, cex=NULL, col=NULL, pch=NULL, bg=NULL,
                              mar=NULL, mgp=NULL,
                              eos="gsw",
                              TScontrol=list(),
                              debug=0,
                              ...)
          {
              if (!requireNamespace("oce", quietly=TRUE))
                  stop("must install.packages(\"oce\") for plot() to work")
              debug <- if (debug > 2) 2 else max(0, floor(debug + 0.5))
              argoFloatsDebug(debug, "plot(x, which=\"", which, "\") {\n", sep="", unindent=1, style="bold")
              if (!inherits(x, "argoFloats"))
                  stop("In plot() : method is only for objects of class \"argoFloats\"", call.=FALSE)
              if (length(which) != 1)
                  stop("\"which\" must contain only one item")
              if (!geographical %in% c(0, 1, 4))
                  stop("In plot() : \"geographical\" must be 0, 1, or 4", call.=FALSE)
              omgp <- par("mgp")
              if (is.null(mgp))
                  mgp <- c(2, 0.7, 0)
              omar <- par("mar")
              if (is.null(mar))
                  mar <- par("mar") #c(mgp[1] + 1.5, mgp[1] + 1.5, mgp[1], mgp[1])
              par(mar=mar, mgp=mgp)
              if (which == "map") {
                  argoFloatsDebug(debug, "map plot\n", sep="")
                  longitude <- x[["longitude", debug=debug]]
                  latitude <- x[["latitude", debug=debug]]
                  if (is.null(xlim))
                      xlim <- extendrange(longitude)
                  if (is.null(ylim))
                      ylim <- extendrange(latitude)

                  ## Draw empty plot box, with axes, to set par("usr") for later use with bathymetry.
                  xlab <- if (is.null(xlab)) "" else xlab
                  ylab <- if (is.null(ylab)) "" else ylab
                  ## Decode bathymetry
                  if (is.logical(bathymetry)) {
                      drawBathymetry <- bathymetry
                      bathymetry <- list(source="auto", keep=TRUE, contour=FALSE, colormap="auto", palette=TRUE)
                  } else if (is.list(bathymetry)) {
                      drawBathymetry <- TRUE
                      if (!("source" %in% names(bathymetry)))
                          stop("In plot() : \"bathymetry\" is a list, it must contain \"source\", at least", call.=FALSE)
                      if (is.null(bathymetry$keep))
                          bathymetry$keep <- TRUE
                      if (is.null(bathymetry$contour))
                          bathymetry$contour <- FALSE
                      if (is.null(bathymetry$colormap))
                          bathymetry$colormap <- "auto"
                      if (is.null(bathymetry$palette))
                          bathymetry$palette <- TRUE
                  } else {
                      stop("In plot() : \"bathymetry\" must be logical, an object created by marmap::getNOAA.bathy(), or a list", call.=FALSE)
                  }
                  if (!is.logical(bathymetry$keep))
                      stop("In plot() : \"bathymetry$keep\" must be a logical value", call.=FALSE)
                  if (!is.logical(bathymetry$palette))
                      stop("In plot() : \"bathymetry$palette\" must be a logical value", call.=FALSE)
                  argoFloatsDebug(debug, "drawBathymetry calculated to be ", drawBathymetry, "\n", sep="")
                  asp <- 1 / cos(pi/180*mean(range(latitude, na.rm=TRUE)))
                  argoFloatsDebug(debug, "asp=", asp, "\n", sep="")
                  if (drawBathymetry) {
                      argoFloatsDebug(debug, "handling bathymetry\n", sep="")
                      if (!requireNamespace("marmap", quietly=TRUE))
                          stop("must install.packages(\"marmap\") to plot with bathymetry")
                      ## Handle bathymetry file downloading (or the use of a supplied value)
                      bathy <- NULL
                      if (is.character(bathymetry$source) && bathymetry$source == "auto") {
                          argoFloatsDebug(debug, "must either download bathymetry or use existing file\n", sep="")
                          argoFloatsDebug(debug, "  before using plot.window(), mar=c(", paste(round(mar, 4), collapse=", "), ")\n", sep="")
                          ## Do plot calculations so we will know usr, needed to determine
                          ## range of longitude and latitude for downloading. Note that we
                          ## need to set mar temporarily, to match what will later be used
                          ## for the actual plot.
                          ## > A<-par("mar")
                          ## > drawPalette()
                          ## > B<-par("mar")
                          ## > A-B
                          ## [1] -8.881784e-16  0.000000e+00  0.000000e+00 -2.750000e+00
                          if (!bathymetry$contour && bathymetry$palette) {
                              tmpmar <- par("mar")
                              par(mar=tmpmar + c(0, 0, 0, 2.75))
                              argoFloatsDebug(debug, "  temporarily set par(mar=c(", paste(par("mar"), collapse=", "), ")) to allow for the palette\n", sep="")
                          }
                          argoFloatsDebug(debug, "  using plot.window() to determine area for bathymetry download, with\n",
                                          "    extendrange(longitude)=c(", paste(extendrange(longitude), collapse=","), ")\n",
                                          "    extendrange(latitude)=c(", paste(extendrange(latitude), collapse=","), ")\n",
                                          "    xlim=c(", paste(ylim, collapse=","), ")\n",
                                          "    ylim=c(", paste(ylim, collapse=","), ")\n",
                                          "    asp=", asp, "\n", sep="")
                          plot.window(xlim=xlim, ylim=ylim,
                                      xaxs="i", yaxs="i",
                                      asp=asp,
                                      xlab=xlab, ylab=ylab)
                          if (!bathymetry$contour && bathymetry$palette) {
                              argoFloatsDebug(debug, "setting mar to ", paste(tmpmar, collapse=" "), "\n")
                              par(mar=tmpmar)
                          }
                          usr <- par("usr")
                          argoFloatsDebug(debug, "  after using plot.window(), usr=c(", paste(round(usr, 4), collapse=", "), ")\n", sep="")
                          latitudeSpan <- usr[4] - usr[3]
                          Dlon <- (usr[2] - usr[1]) / 20
                          Dlat <- (usr[4] - usr[3]) / 20
                          ## resolution <- ifelse(latitudeSpan < 5, 1,
                          ##                      ifelse(latitudeSpan < 20, 2,
                          ##                             ifelse(latitudeSpan < 90, 8, 60)))
                          resolution <- as.integer(round(1 + 60 * latitudeSpan / 400))
                          argoFloatsDebug(debug, "  Dlat=", round(Dlat, 4), "\n", sep="")
                          argoFloatsDebug(debug, "  Dlon=", round(Dlon, 4), "\n", sep="")
                          argoFloatsDebug(debug, "  resolution=", resolution, "\n", sep="")
                          argoFloatsDebug(debug, "  minlon=", round(usr[1]-Dlon, 3), "\n", sep="")
                          argoFloatsDebug(debug, "  maxlon=", round(usr[2]+Dlon, 3), "\n", sep="")
                          argoFloatsDebug(debug, "  minlat=", round(usr[3]-Dlat, 3), "\n", sep="")
                          argoFloatsDebug(debug, "  maxlat=", round(usr[4]+Dlat, 3), "\n", sep="")
                          ## Round to 4 digits to prevent crazy filenames for no good reason
                          bathy <- try(marmap::getNOAA.bathy(max(-180, round(usr[1]-Dlon, 3)),
                                                             min(+180, round(usr[2]+Dlon, 3)),
                                                             max(-90, round(usr[3]-Dlat, 3)),
                                                             min(+90, round(usr[4]+Dlat, 3)),
                                                             resolution=resolution,
                                                             keep=bathymetry$keep),
                                       silent=FALSE)
                          argoFloatsDebug(debug, "  grid size", paste(dim(bathy), collapse="x"), "\n")
                          if (inherits(bathymetry, "try-error"))
                              warning("could not download bathymetry from NOAA server\n")
                          argoFloatsDebug(debug, "  bathy span=", min(bathy, na.rm=TRUE), " to ", max(bathy, na.rm=TRUE), "\n", sep="")
                      } else if (inherits(bathymetry$source, "bathy")) {
                          argoFloatsDebug(debug, "using supplied bathymetry$source\n", sep="")
                          bathy <- bathymetry$source
                      } else if (inherits(bathymetry$source, "oce") &&
                                 inherits(bathymetry$source, "topo")) {
                          argoFloatsDebug(debug, "using oce-style topo object (converted to NOAA bathy)\n", sep="")
                          ##browser()
                          dim <- dim(bathymetry$source[["z"]])
                          bathy <- matrix(as.integer(bathymetry$source[["z"]]), nrow=dim[1], ncol=dim[2])
                          rownames(bathy) <- bathymetry$source[["longitude"]]
                          colnames(bathy) <- bathymetry$source[["latitude"]]
                          class(bathy) <- "bathy"
                      } else {
                          stop("cannot determine bathymetry data source")
                      }
                      ## Handle colormap
                      if (!bathymetry$contour && is.character(bathymetry$colormap) && length(bathymetry$colormap) == 1 && bathymetry$colormap == "auto") {
                          argoFloatsDebug(debug, "setting a default colormap for 0m to ", -min(bathy), "m depth\n")
                          bathymetry$colormap <- oce::colormap(zlim=c(0, -min(bathy)),
                                                               col=function(...)
                                                                   rev(oce::oceColorsGebco(...)))
                      } else {
                          argoFloatsDebug(debug, "using supplied bathymetry$colormap\n")
                      }
                      ## Handle optional drawing of the palette. Note space for one line of text is removed
                      ## from the LHS of the palette and added to the RHS.  This is because we know that
                      ## there is no axis to the left of the palette, so we do not need space for one.
                      ## We recover the stolen space by putting it back at the RHS, where it can be
                      ## useful, especially if a map plot has another plot to its right.
                      if (!bathymetry$contour && bathymetry$palette) {
                          argoFloatsDebug(debug, "drawing a bathymetry palette\n")
                          ## Increase space to right of axis, decreasing equally to the left.
                          textHeight <- par("cin")[2]
                          mai <- c(0, -textHeight, 0, textHeight)
                          oce::drawPalette(colormap=bathymetry$colormap, mai=mai)
                      } else {
                          argoFloatsDebug(debug, "not drawing a bathymetry palette, as instructed\n")
                      }
                  }
                  argoFloatsDebug(debug, "initial xlim=",
                                  "c(", paste(xlim, collapse=","), ") and ylim=",
                                  "c(", paste(ylim, collapse=","), ")\n", sep="")
                  ## Extend to world edge, if we are close
                  if (xlim[1] < -170) xlim[1] <- -180
                  if (xlim[2] >  170) xlim[2] <-  180
                  if (ylim[1] < -70) ylim[1] <- -90
                  if (ylim[2] >  70) ylim[2] <-  90
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
                      stop("In plot() : programming error: \"geographical\" must be 0, 1, or 4", call.=FALSE)
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
                                  -bathy,
                                  breaks=c(100, 200, 500, seq(1e3, 10e3, 1e3)),
                                  labels=c("100m", "200m", "500m", paste(1:10, "km", sep="")),
                                  labcex=0.9, add=TRUE)
                      } else {
                          argoFloatsDebug(debug, "indicating bathymetry with an image\n")
                          oce::imagep(as.numeric(rownames(bathy)),
                                      as.numeric(colnames(bathy)),
                                      -bathy, colormap=bathymetry$colormap, add=TRUE)
                      }
                      rect(usrTrimmed[1], usrTrimmed[3], usrTrimmed[2], usrTrimmed[4], lwd=1)
                  }
                  points(unlist(longitude), unlist(latitude),
                         cex=if (is.null(cex)) 1 else cex,
                         col=if (is.null(col)) "white" else col,
                         pch=if (is.null(pch)) 21 else pch,
                         bg=if (is.null(bg)) "red" else bg,
                         ...)
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
                          polygon(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]], col="tan")
                      } else if (mapSpan < C / 4) {
                          argoFloatsDebug(debug, "using coastlineWorldMedium from ocedata package\n")
                          data("coastlineWorldMedium", package="ocedata", envir=environment())
                          coastlineWorldMedium <- get("coastlineWorldMedium")
                          polygon(coastlineWorldMedium[["longitude"]], coastlineWorldMedium[["latitude"]], col="tan")
                      } else {
                          argoFloatsDebug(debug, "using coastlineWorld from oce package, since the span is large\n")
                          data("coastlineWorld", package="oce", envir=environment())
                          coastlineWorld <- get("coastlineWorld")
                          polygon(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]], col="tan")
                      }
                  } else {
                      argoFloatsDebug(debug, "using coastlineWorld from oce package, since the ocedata package is not installedi\n")
                      data("coastlineWorld", package="ocedata", envir=environment())
                      coastlineWorld <- get("coastlineWorld")
                      polygon(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]], col="tan")
                  }
                  par(mar=omar, mgp=omgp)
              } else if (which == "TS") {
                  argoFloatsDebug(debug, "TS plot\n", sep="")
                  if ((x[["type"]] != "argos"))
                      stop("In plot,argoFloats-method(): : x must have been created by readProfiles()", call.=FALSE)
                  if (!(eos %in% c("gsw", "unesco")))
                      stop("In plot,argoFloats-method(): \n eos must be \"gsw\" or \"unesco\", not \"", eos, "\"", call.=FALSE)
                  salinity <- unlist(x[["salinity", debug=debug]])
                  temperature <- unlist(x[["temperature", debug=debug]])
                  pressure <- unlist(x[["pressure", debug=debug]])
                  ## Use byLevel to repeat the latitude and longitude values across
                  ## the depths in each profile, so that the resultant vector
                  ## will match the salinity, temperature and pressure vectors.
                  latitude <- unlist(x[["latitude", "byLevel", debug=debug]])
                  longitude <- unlist(x[["longitude", "byLevel", debug=debug]])
                  ctd <- oce::as.ctd(salinity=salinity,
                                     temperature=temperature,
                                     pressure=pressure,
                                     latitude=latitude,
                                     longitude=longitude)
                  if (is.null(cex))
                      cex <- 0.5
                  if (is.null(TScontrol$colByCycle)) {
                      if (is.null(col)) {
                          if (which == "TS") {
                              col <- "flags"
                          } else {
                              col <- rgb(0, 0, 1, 0.5)
                          }
                      }
                  } else {
                      ## Ignore "col" if TScontrol contains "colByCycle"
                      colByCycle <- TScontrol$colByCycle
                      cycle <- unlist(x[["cycle", debug=debug]])
                      lengths <- sapply(x[["argos"]], function(cc) length(cc[["pressure"]]))
                      ## Increase the col length, so e.g. TScontrol=list(colByCycle=1:2) will alternate colours
                      colByCycle <- rep(colByCycle, length.out=length(cycle))
                      col <- unlist(lapply(seq_along(cycle), function(i) rep(colByCycle[i], lengths[i])))
                  }
                  if (is.null(pch))
                      pch <- 20
                  omgp <- par("mgp")
                  if (is.null(mgp))
                      mgp <- c(2, 0.7, 0)
                  omar <- par("mar")
                  if (is.null(mar))
                      mar <- par("mar") # c(mgp[1] + 1.5, mgp[1] + 1.5, mgp[1], mgp[1])
                  par(mar=mar, mgp=mgp)
                  if (col[1] == "flags") {
                      argoFloatsDebug(debug, "col is \"flags\"\n")
                      salinityFlag <- unlist(x[["salinityFlag"]])
                      temperatureFlag <- unlist(x[["temperatureFlag"]])
                      goodT <- temperatureFlag %in% c(1, 2, 5, 8)
                      goodS <- salinityFlag %in% c(1, 2, 5, 8)
                      good <- goodS & goodT
                      col <- ifelse(good, "black", "red")
                      if (pch == 21)
                          bg <- ifelse(good, "black", "red")
                  }
                  oce::plotTS(ctd, cex=cex, bg=bg, col=col, pch=pch, mar=mar, mgp=mgp, eos=eos, ...)
                  par(mar=omar, mgp=omgp)
              } else if (which == "QC") {
                  if (x[["type"]] != "argos")
                      stop("In plot,argoFloats-method(): The type of x must be \"argos\"", call.=FALSE)
                  ids <- x[["id"]]
                  nid <- length(unique(ids))
                  if (nid != 1)
                      stop("In plot,argoFloats-method(): It is only possible to plot a QC of a single id", call.=FALSE)
                  dots <- list(...)
                  knownParameters <- names(x[[1]]@metadata$flags) # FIXME: is it possible that later cycles have different flags?
                  parameter <- dots$parameter
                  if (is.null(parameter))
                      stop("In plot,argoFloats-method(): Please provide a parameter, one of \"", paste(sort(knownParameters), collapse="\", \""), "\"", call.=FALSE)
                  if (!(parameter %in% knownParameters))
                      stop("In plot,argoFloats-method(): Parameter '", parameter, "' not found. Try one of: \"", paste(sort(knownParameters), collapse="\", \""), "\"", call.=FALSE)
                  qf <- function(x) {
                      # qf returns 100 if data are all "good" = 1 or "probably good" = 2 or "changed" = 5 or "estimated" = 8
                      flag <- x[[paste0(parameter, "Flag")]]
                      100 * sum(1 == flag | 2 == flag | 5 == flag | 8 == flag, na.rm=TRUE) / length(flag)
                  }
                  meanf <- function(x)
                      mean(x[[parameter]], na.rm=TRUE)
                  time <- oce::numberAsPOSIXct(unlist(lapply(x[["argos"]], function(x) x[["time"]])))
                  for (parameter in parameter) {
                      q <- unlist(lapply(x[["argos"]], qf))
                      m <- unlist(lapply(x[["argos"]], meanf))
                      par(mfrow=c(2,1), mar=c(2.5,2.5,1,1))
                      if (any(is.finite(q))) {
                          o <- order(time) # cycles are not time-ordered in index files
                          oce::oce.plot.ts(time[o], q[o], ylab=paste(parameter, "% Good"), drawTimeRange = FALSE, type="l")
                          points(time[o], q[o], col=ifelse(q[o] < 50, "red", "black"), pch=20, cex=1)
                          abline(h=50, col="red", lty="dashed")
                          oce::oce.plot.ts(time[o], m[o], ylab=paste(parameter, "Mean"), drawTimeRange = FALSE, type="l")
                          points(time[o], m[o], col=ifelse(q[o] < 50, "red", "black"), pch=20, cex=1)
                      } else {
                          plot(0:1, 0:1, xlab="", ylab='', type="n", axes=FALSE)
                          box()
                          text(0, 0.5, paste(' No', parameter, "flags available"), pos=4)
                          plot(0:1, 0:1, xlab="", ylab='', type="n", axes=FALSE)
                          box()
                          text(0, 0.5, paste(' No', parameter, "flags available"), pos=4)
                      }
                  }
              } else if (which == "profile") {
                  if (x[["type"]] != "argos")
                      stop("In plot,argoFloats-method(): The type of x must be \"argos\"", call.=FALSE)
                  dots <- list(...)
                  N <- length(x[["argos"]])
                  ## The known parameter names include not just the things stored in the
                  ## data slot (of *any* of the profiles), but also some computable things. We
                  ## will always have longitude and latitude, so there's no need to check for them.
                  ## A core profile will always get the computable things, but bgc profiles
                  ## may lack e.g. salinity and temperature, so all we need to check for is
                  ## salinity, temperature, and pressure.
                  knownParameters <- unique(unlist(lapply(1:N, function(i) names(x[[i]][["data"]]))))
                  if (all(c("salinity", "temperature", "pressure") %in% knownParameters)) {
                      knownParameters <- c(knownParameters,
                                           "SA", "CT", "sigma0", "sigma1", "sigma2", "sigma3", "sigma4", "sigmaTheta",
                                           "theta",
                                           "spice",
                                           "N2")
                  }
                  argoFloatsDebug(debug, "knownParameters: \"", paste(sort(knownParameters), collapse="\", \""), "\".\n", sep="")
                  #print(sort(knownParameters))
                  parameter <- dots$parameter
                  N <- length(x[["argos"]])
                  if (is.null(parameter))
                      stop("In plot,argoFloats-method(): Please provide a parameter, one of ", paste(knownParameters, collapse=', '), call.=FALSE)
                  if (!(parameter %in% knownParameters))
                      stop("In plot,argoFloats-method(): parameter=\"", parameter, "\" is not in the dataset, or calculable from that dataset; try one of the following: \"", paste(sort(knownParameters), collapse='", "'), "\".", call.=FALSE)
                  if ((parameter %in% knownParameters)) {
                      argoFloatsPlotProfile <- function(x, parameter, ...)
                      {
                          pressure <- lapply(1:N, function(i) x[[i]][["pressure"]])
                          variable <- lapply(1:N, function(i) x[[i]][[parameter]])
                          nn <- unlist(lapply(1:N, function(i) prod(dim(x[[i]][[parameter]]))))
                          pp <- NULL
                          vv <- NULL
                          ## OLD: punit <- NULL
                          ## OLD: vunit <- NULL
                          argoFloatsDebug(debug, "number of profiles: ", N, "\n")
                          for (i in seq_len(N)) {
                              if (nn[i] > 0) {
                                  ## OLD: if (is.null(vunit))
                                  ## OLD:     vunit <- x[[1]][[paste0(parameter, "Unit")]]
                                  ## OLD: if (is.null(punit))
                                  ## OLD:     punit <- x[[1]][[paste0("pressureUnit")]]
                                  pp <- c(pp, NA, pressure[[i]])
                                  vv <- c(vv, NA, variable[[i]])
                              }
                              ## cat(vectorShow(i))
                              ## cat(vectorShow(length(pp)))
                              ## cat(vectorShow(length(vv)))
                          }
                          ## OLD: o <- new("ctd")
                          ## OLD: o <- oce::oceSetData(o, "pressure", pp, unit=punit)
                          ## OLD: o <- oce::oceSetData(o, parameter, vv, unit=vunit)
                          plot(vv, pp, ylim=rev(range(pp, na.rm=TRUE)),
                               axes=FALSE,
                               ylab="", xlab="", # draw axes later, in oceanographic 'profile' locations
                               cex=cex,
                               type=if(is.null(type)) "l" else type,
                               col=if (is.null(col)) par("col") else col,
                               pch=pch, ...)
                          box()
                          axis(2)
                          axis(3)
                          mtext(oce::resizableLabel("p"), side=2, line=par("mgp")[1], cex=par("cex"))
                          mtext(oce::resizableLabel(parameter), side=3, line=par("mgp")[1], cex=par("cex"))
                          ## OLD: if ("keepNA" %in% names(list(...))) {
                          ## OLD:     oce::plotProfile(o, xtype=parameter, cex=cex,
                          ## OLD:                      type=if(is.null(type)) "l" else type,
                          ## OLD:                      col=if (is.null(col)) par("col") else col,
                          ## OLD:                      pch=pch, ...)
                          ## OLD: } else {
                          ## OLD:     message("DAN 1")
                          ## OLD:     DANo<<-o
                          ## OLD:     DANparameter<<-parameter
                          ## OLD:     oce::plotProfile(o, keepNA=TRUE, xtype=parameter, cex=cex,
                          ## OLD:                      type=if(is.null(type)) "l" else type,
                          ## OLD:                      col=if (is.null(col)) par("col") else col,
                          ## OLD:                      pch=pch, ...)
                          ## OLD:     message("DAN 2")
                          ## OLD: }
                      }
                  }
                  argoFloatsPlotProfile(x, parameter=parameter)
              } else {
                  stop("In plot,argoFloats-method():cannot handle which=\"", which, "\"; see ?\"plot,argoFloats-method\"", call.=FALSE)
              }
              argoFloatsDebug(debug, "} # plot()\n", sep="", unindent=1, style="bold")
          }
)
