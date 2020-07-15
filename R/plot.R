## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

geographical <- TRUE

#' Plot an argoFloats object
#'
#' The action depends on the `type` of the object, and
#' this is set up by the function that created the object;
#' see \dQuote{Details}.
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
#'        a. `source`, a mandatory value that either the string `"auto"` (the default) to use
#'           [marmap::getNOAA.bathy()] to download the data, or a value
#'           returned by a previous call to that function;
#'        b. `keep`, an optional logical value (with `TRUE` as the default) that is passed to
#'           [marmap::getNOAA.bathy()] to indicate whether to keep a local file of bathymetry,
#'           as a way to avoid intermittent problems with the NOAA server;
#'        c. `colormap`, an optional value that is either the string `"auto"` (the default)
#'           for a form of GEBCO colors computed with [oce::oceColorsGebco()], or a value
#'           computed with [oce::colormap()] applied to the bathymetry data; and
#'        d. `palette`, an optional logical value (with `TRUE` as the default)
#'           indicating whether to draw a depth-color palette to the right of the plot.
#'
#' * For `which="TS"`,  an overall TS plot is created.  This
#' only works if `x` is an object that was created by  [getProfiles()].
#' The scales for the plot can be altered by putting `Slim` and `Tlim`
#' arguments in the `...` list; see the documentation for [oce::plotTS()]
#' for other arguments that can be provided.
#'
#' * For `which=\"QC\"`, two time-series panels are shown, with
#' time being that recorded in the individual profile in the dataset.
#' An additional argument named `variable` must be givn, to name the
#' quantity of interest.  The function only works if `x` is an
#' [`argoFloats-class`] object creatd with [readProfiles()].
#' The top panel shows the percent of data flagged with codes
#' 1 (meaning good data), 2 (probably good), 5 (changed)
#' or 8 (estimated).  Thus, low values on the top panel reveal
#' profiles that are questionable. The bottom panel shows the mean value
#' of the parameter in question.  See Example 7.
#'
#' @param x An [`argoFloats-class`] object.
#'
#' @param which A string that indicates the type of plot; see \dQuote{Details}.
#'
#' @param bathymetry An argument used only if `which="map"`, to control
#' whether (and how) to indicate water depth; see `\dQuote{Details}.
#'
#' @param xlim,ylim Limits of plot axes, as for [plot.default()] and other conventional
#' plotting functions.
#'
#' @param xlab A character value indicating the name for the horizontal axis, or
#' `NULL`, which indicates that this function should choose an appropriate name
#' depending on the value of `which`. Note that `xlab` is not obeyed if
#' `which="TS"`, because altering that label can be confusing to the user.
#'
#' @param ylab As `xlab`, but for the vertical axis.
#'
#' @param cex A character expansion factor for plot symbols, or `NULL`, to get an
#' value that depends on the value of `which`.
#'
#' @param col The colour to be used for plot symbols, or `NULL`, to get an value
#' that depends on the value of `which`.
#' (See [par()] for more on specifying `pch`.)
#'
#' @param bg The colour to be used for plot symbol interior, for `pch`
#' values that distinguish between the interior of the symbol and the
#' border, e.g. for `pch=21`.
#'
#' @param pch An integer or code indicating the type of plot symbol, or `NULL`,
#' to get a value that depends on the value of `which`.
#' (See [par()] for more on specifying `pch`.)
#'
#' @param mar Either a four-element vector giving the margins to be used for
#' the plot (see [par()] for details), or `NULL`, which means to use
#' [par]`("mar")`.
#'
#' @param mgp Either a three-element vector giving the geometry for
#' axis labels (see [par()] for details), or `NULL`, which means to use
#' [par]`("mgp")`.
#'
#' @param eos A character value indicating the equation of state to use
#' if `which="TS"`.  This must be `"gsw"` (the default) or `"unesco"`;
#' see [oce::plotTS()].
#'
#' @param debug An integer specifying the level of debugging.
#'
#' @param \dots Extra arguments passed to the plot calls that are made
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
#' par(mar=c(3, 3, 1, 1))
#' # Note that colormap shows water depth, not elevation above sea level
#' bathy <- marmap::getNOAA.bathy(-82, -71, 23, 30, 2, keep=TRUE)
#' cm <- colormap(zlim=c(0, -min(bathy)), col=function(...) rev(oceColorsGebco(...)))
#' plot(index, bathymetry=list(source=bathy, keep=TRUE, colormap=cm, palette=TRUE))}
#'
#' # Example 6: TS plot for a particular argo
#' library(argoFloats)
#' a <- readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats"))
#' plot(a[[1]], which="TS")
#'
#' # Example 7: Temperature QC plot for an ID in Arabian Sea
#' \dontrun{
#' library(argoFloats)
#' ais <- getIndex(filename='synthetic', age=0)
#' sub <- subset(ais, ID='2902123')
#' lonRect <- c(56, 66)
#' latRect <- c(11,12)
#' s <- subset(sub, rectangle=list(longitude=lonRect, latitude=latRect))
#' profiles <- getProfiles(s)
#' argos <- readProfiles(profiles)
#' plot(argos, which='QC', parameter='temperature')}
#'
#' @importFrom grDevices extendrange gray rgb
#' @importFrom graphics abline axis box par plot.window points polygon text
#' @importFrom utils data
## @importFrom oce as.ctd colormap drawPalette imagep oceColorsGebco oce.plot.ts plotTS
## @importFrom marmap getNOAA.bathy
#' @export
#' @aliases plot,argoFloats-method
#' @author Dan Kelley
setMethod(f="plot",
          signature=signature("argoFloats"),
          definition=function(x,
                              which="map",
                              bathymetry=TRUE,
                              xlim=NULL, ylim=NULL,
                              xlab=NULL, ylab=NULL,
                              cex=NULL, col=NULL, pch=NULL, bg=NULL,
                              mar=NULL, mgp=NULL,
                              eos="gsw",
                              debug=0,
                              ...)
          {
              if (!requireNamespace("oce", quietly=TRUE))
                  stop("must install.packages(\"oce\") for plot() to work")
              debug <- if (debug > 2) 2 else max(0, floor(debug + 0.5))
              argoFloatsDebug(debug, "plot(x, which=\"", which, "\") {\n", sep="", unindent=1)
              if (!inherits(x, "argoFloats"))
                  stop("In plot() : method is only for objects of class 'argoFloats'", call.=FALSE)
              if (length(which) != 1)
                  stop("'which' must contain only one item")
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
                  
                  ## Draw empty plot box, with axes, to set par("usr") for later use with bathymetry.
                  if (geographical) {
                      xlab <- if (is.null(xlab)) "" else xlab
                      ylab <- if (is.null(ylab)) "" else ylab
                  } else {
                      xlab <- if (is.null(xlab)) "Longitude" else xlab
                      ylab <- if (is.null(ylab)) "Latitude" else ylab
                  }
                  
                  ## Decode bathymetry
                  if (is.logical(bathymetry)) {
                      drawBathymetry <- bathymetry
                      bathymetry <- list(source="auto", keep=TRUE, colormap="auto", palette=TRUE)
                  } else if (is.list(bathymetry)) {
                      drawBathymetry <- TRUE
                      if (!("source" %in% names(bathymetry)))
                          stop("In plot() : 'bathymetry' is a list, it must contain 'source', at least", call.=FALSE)
                      if (is.null(bathymetry$keep))
                          bathymetry$keep <- TRUE
                      if (is.null(bathymetry$colormap))
                          bathymetry$colormap <- "auto"
                      if (is.null(bathymetry$palette))
                          bathymetry$palette <- TRUE
                  } else {
                      stop("In plot() : 'bathymetry' must be logical, an object created by marmap::getNOAA.bathy(), or a list", call.=FALSE)
                  }
                  if (!is.logical(bathymetry$keep))
                      stop("In plot() : 'bathymetry$keep' must be a logical value", call.=FALSE)
                  if (!is.logical(bathymetry$palette))
                      stop("In plot() : 'bathymetry$palette' must be a logical value", call.=FALSE)
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
                          if (bathymetry$palette) {
                              tmpmar <- par("mar")
                              par(mar=tmpmar + c(0, 0, 0, 2.75))
                              argoFloatsDebug(debug, "  temporarily set par(mar=c(", paste(par("mar"), collapse=", "), ")) to allow for the palette\n", sep="")
                          }
                          if (!is.null(xlim) && !is.null(ylim)) {
                              argoFloatsDebug(debug, "  using plot.window() to determine area for bathymetry download, with\n",
                                              "    extendrange(longitude)=c(", paste(extendrange(longitude), collapse=","), ")\n",
                                              "    extendrange(latitude)=c(", paste(extendrange(latitude), collapse=","), ")\n",
                                              "    xlim=c(", paste(ylim, collapse=","), ")\n",
                                              "    ylim=c(", paste(ylim, collapse=","), ")\n",
                                              "    asp=", asp, "\n", sep="")
                              plot.window(extendrange(longitude), extendrange(latitude),
                                          xlim=xlim, ylim=ylim,
                                          xaxs="i", yaxs="i",
                                          asp=asp,
                                          xlab=xlab, ylab=ylab)
                          } else {
                              argoFloatsDebug(debug, "  using plot.window() to determine area for bathymetry download, with\n",
                                              "    extendrange(longitude)=c(", paste(extendrange(longitude), collapse=","), ")\n",
                                              "    extendrange(latitude)=c(", paste(extendrange(latitude), collapse=","), ")\n",
                                              "    asp=", asp, "\n", sep="")
                              plot.window(extendrange(longitude), extendrange(latitude),
                                          xaxs="i", yaxs="i",
                                          asp=asp,
                                          xlab=xlab, ylab=ylab)
                          }
                          if (bathymetry$palette) {
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
                      } else {
                          stop("cannot determine bathymetry data source")
                      }
                      ## Handle colormap
                      if (is.character(bathymetry$colormap) && length(bathymetry$colormap) == 1 && bathymetry$colormap == "auto") {
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
                      if (bathymetry$palette) {
                          argoFloatsDebug(debug, "drawing a bathymetry palette\n")
                          ## Increase space to right of axis, decreasing equally to the left.
                          textHeight <- par("cin")[2]
                          mai <- c(0, -textHeight, 0, textHeight)
                          oce::drawPalette(colormap=bathymetry$colormap, mai=mai)
                      } else {
                          argoFloatsDebug(debug, "not drawing a bathymetry palette, as instructed\n")
                      }
                  }
                  if (geographical) {
                      argoFloatsDebug(debug, "about to use plot(), with xlim=",
                                      "c(", paste(xlim, collapse=","), ") and ylim=",
                                      "c(", paste(ylim, collapse=","), ")\n", sep="")
                      if (!is.null(xlim) && !is.null(ylim)) {
                          argoFloatsDebug(debug, "about to plot, with\n",
                                          "    extendrange(longitude)=c(", paste(extendrange(longitude), collapse=","), ")\n",
                                          "    extendrange(latitude)=c(", paste(extendrange(latitude), collapse=","), ")\n",
                                          "    xlim=c(", paste(ylim, collapse=","), ")\n",
                                          "    ylim=c(", paste(ylim, collapse=","), ")\n",
                                          "    asp=", asp, "\n", sep="")
                          plot(extendrange(longitude), extendrange(latitude),
                               xlim=xlim, ylim=ylim,
                               xaxs="i", yaxs="i",
                               asp=asp,
                               xlab=xlab, ylab=ylab, type="n", axes=FALSE)
                      } else {
                          argoFloatsDebug(debug, "about to plot, with\n",
                                          "    extendrange(longitude)=c(", paste(extendrange(longitude), collapse=","), ")\n",
                                          "    extendrange(latitude)=c(", paste(extendrange(latitude), collapse=","), ")\n",
                                          "    asp=", asp, "\n", sep="")
                          plot(extendrange(longitude), extendrange(latitude),
                               xaxs="i", yaxs="i",
                               asp=asp,
                               xlab=xlab, ylab=ylab, type="n", axes=FALSE)
                      }
                      argoFloatsDebug(debug, "after plot(), usr=c(", paste(round(usr, 4), collapse=", "), ")\n", sep="")
                      xaxp <- par("xaxp")
                      xat <- seq(xaxp[1], xaxp[2], length.out=xaxp[3]+1)
                      xlabel <- paste(abs(xat), ifelse(xat < 0, "W", ifelse(xat > 0, "E", "")), sep="")
                      axis(1, at=xat, labels=xlabel)
                      yaxp <- par("yaxp")
                      yat <- seq(yaxp[1], yaxp[2], length.out=yaxp[3]+1)
                      ylabel <- paste(abs(yat), ifelse(yat < 0, "S", ifelse(yat > 0, "N", "")), sep="")
                      axis(2, at=yat, labels=ylabel)
                      box()
                  } else {
                      if (!is.null(xlim) && !is.null(ylim)) {
                          plot(extendrange(longitude), extendrange(latitude),
                               xlim=xlim, ylim=ylim,
                               xaxs="i", yaxs="i",
                               asp=asp,
                               xlab=xlab, ylab=ylab, type="n")
                      } else {
                          plot(extendrange(longitude), extendrange(latitude),
                               xaxs="i", yaxs="i",
                               asp=asp,
                               xlab=xlab, ylab=ylab, type="n")
                      }
                  }
                  if (drawBathymetry)
                      oce::imagep(as.numeric(rownames(bathy)),
                                  as.numeric(colnames(bathy)),
                                  -bathy, colormap=bathymetry$colormap, add=TRUE)
                  points(longitude, latitude,
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
                      l <- oce::geodDist(usr[1], usr[3], usr[1], usr[4]) # length [km] on left margin
                      r <- oce::geodDist(usr[2], usr[3], usr[2], usr[4]) # length [km] on right margin
                      b <- oce::geodDist(usr[1], usr[1], usr[2], usr[1]) # length [km] on bottom margin
                      t <- oce::geodDist(usr[1], usr[4], usr[2], usr[4]) # length [km] on top margin
                      mapSpan <- max(l, r, b, t) # largest length [km]
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
                      stop("eos must be \"gsw\" or \"unesco\", not \"", eos, "\"")
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
                  if (is.null(col))
                      col <- rgb(0, 0, 1, 0.5)
                  if (is.null(pch))
                      pch <- 20
                  omgp <- par("mgp")
                  if (is.null(mgp))
                      mgp <- c(2, 0.7, 0)
                  omar <- par("mar")
                  if (is.null(mar))
                      mar <- par("mar") # c(mgp[1] + 1.5, mgp[1] + 1.5, mgp[1], mgp[1])
                  par(mar=mar, mgp=mgp)
                  oce::plotTS(ctd, cex=cex, col=col, pch=pch, mar=mar, mgp=mgp, eos=eos, ...)
                  par(mar=omar, mgp=omgp)
              } else if (which == "QC") {
                  if (x[['type']] != 'argos')
                      stop("In plot,argoFloats-method(): The type of x must be 'argos'", call.=FALSE)
                  IDs <- x[['ID']]
                  nID <- length(unique(IDs))
                  if (nID != 1)
                      stop("In plot,argoFloats-method(): It is only possible to plot a QC of a single ID", call.=FALSE)
                  dots <- list(...)
                  knownParameters <- names(x[[1]]@metadata$flags)
                  parameter <- dots$parameter
                  if (is.null(parameter))
                      stop("In plot,argoFloats-method(): Please provide a parameter, one of ", paste(knownParameters, collapse=', '), call.=FALSE)
                  if (!(parameter %in% knownParameters))
                      stop("In plot,argoFloats-method(): Parameter '", parameter, "' not found. Try one of: ", paste(knownParameters, collapse=', '), call.=FALSE)
                  qf <- function(x) {
                      # qf returns 100 if data are all 'good' = 1 or 'probably good' = 2
                      flag <- x[[paste0(parameter, 'Flag')]]
                      100 * sum(1 == flag | 2 == flag | 5 == flag | 8 == flag, na.rm=TRUE) / length(flag)
                  }
                  meanf <- function(x)
                      mean(x[[parameter]], na.rm=TRUE)
                  time <- oce::numberAsPOSIXct(unlist(lapply(x[['profile']], function(x) x[['time']])))
                  for (parameter in parameter) {
                      q <- unlist(lapply(x[['profile']], qf))
                      m <- unlist(lapply(x[['profile']], meanf))
                      par(mfrow=c(2,1), mar=c(2.5,2.5,1,1))
                      if (any(is.finite(q))) {
                          oce::oce.plot.ts(time,q, ylab=paste(parameter, "% Good"), drawTimeRange = FALSE, type='l')
                          points(time, q, col=ifelse(q < 50, 'red', 'black'), pch=20, cex=0.75)
                          abline(h=50, col='red', lty='dashed')
                          oce::oce.plot.ts(time, m, ylab=paste(parameter, "Mean"), drawTimeRange = FALSE, type='l')
                          points(time, m, col=ifelse(q < 50, 'red', 'black'), pch=20, cex=0.75)
                      } else {
                          plot(0:1, 0:1, xlab="", ylab='', type="n", axes=FALSE)
                          box()
                          text(0, 0.5, paste(' No', parameter, 'flags available'), pos=4)
                          plot(0:1, 0:1, xlab="", ylab='', type="n", axes=FALSE)
                          box()
                          text(0, 0.5, paste(' No', parameter, 'flags available'), pos=4)
                      }
                  }
              } else {
                  stop("In plot,argoFloats-method():cannot handle which=\"", which, "\"; see ?'plot,argoFloats-method'", call.=FALSE)
              }
              argoFloatsDebug(debug, "} # plot()\n", sep="", unindent=1)
          }
)
