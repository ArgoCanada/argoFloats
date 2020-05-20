## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

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
#'
#' * For `which="TS"`,  an overall TS plot is created.  This
#' only works if `x` is an object that was created by  [getProfiles()].
#' The scales for the plot can be altered by putting `Slim` and `Tlim`
#' arguments in the `...` list; see the documentation for [oce::plotTS()]
#' for other arguments that can be provided.
#'
#' @param x an [`argoFloats-class`] object.
#'
#' @param which a string that indicates the type of plot; see \dQuote{Details}.
#'
#' @param bathymetry one of three possibilities for an image underlay showing bathymetry
#' (i.e. ocean depth), using a colour scheme set up by the `colormap` argument:
#' (1) `FALSE` (the default), indicating not to draw the bathymetry;
#' (2) `TRUE`, causing `plot()` to call [marmap::getNOAA.bathy()]
#' to create a `bathy` object to be drawn, as in Example 4;
#' or
#' (3) a `bathy` object resulting from a previous call to [marmap::getNOAA.bathy()].
#' Option 3 offers robustness against temporary failures to connect with the NOAA
#' server, but it requires a conversion from elevation to depth, as 
#' illustrated in Example 5.
#'
#' @param colormap either `NULL` (the default), indicating that `plot()` should
#' construct a colormap using [oce::colormap()] based on the depth range
#' associated with the `bathymetry` argument, or an object created by
#' [oce::colormap()], as illustrated in Example 5.
#'
#' @param xlab character value indicating the name for the horizontal axis, or
#' `NULL`, which indicates that this function should choose an appropriate name
#' depending on the value of `which`. Note that `xlab` is not obeyed if
#' `which="TS"`, because altering that label can be confusing to the user.
#'
#' @param ylab as `xlab`, but for th vertical axis.
#'
#' @param cex character expansion factor for plot symbols, or `NULL`, to get an
#' value that depends on the value of `which`.
#'
#' @param col colour to be used for plot symbols, or `NULL`, to get an
#' value that depends on the value of `which`.
#'
#' @param bg colour to be used for plot symbol interior, for `pch`
#' values that distinguish between the interior of the symbol and the
#' border, e.g. for `pch=21`.
#'
#' @param pch number indicating the type of plot symbol, or `NULL`, to get an
#' value that depends on the value of `which`.
#'
#' @param mar either a four-element vector giving the margins to be used for
#' the plot (see [par()] for details), or `NULL`, which means to use
#' [par]`("mar")`.
#'
#' @param mgp either a three-element vector giving the geometry for
#' axis labels (see [par()] for details), or `NULL`, which means to use
#' [par]`("mgp")`.
#'
#' @param eos character value indicating the equation of state to use
#' if `which="TS"`.  This must be `"gsw"` (the default) or `"unesco"`;
#' see [oce::plotTS()].
#'
#' @param debug an integer specifying the level of debugging.
#'
#' @param \dots extra arguments passed to the plot calls that are made
#' to within this function. A common use for `which="map"` cases is
#' to set `xlim` and `ylim` to get enlarge the plot region, so a coastline
#' becomes visible.
#'
#' @examples
#' # Example 1: map profiles in index, highlighting a neighborhood of 10
#' library(argoFloats)
#' library(oce)
#' data(index)
#' plot(index) # also, try using bathymetry=FALSE
#' lon <- index[["longitude"]]
#' lat <- index[["latitude"]]
#' dist <- geodDist(lon, lat, -77.06, 26.54)
#' o <- order(dist)
#' index10 <- subset(index, o[1:10])
#' points(index10[["longitude"]], index10[["latitude"]], col=2)
#'
#' # Example 3: TS of first 10 profiles
#' # (Slow, so not run by default.)
#'\dontrun{
#' profiles20 <- getProfiles(index20, destdir="~/data/argo")
#' argos20 <- readProfiles(profiles20)
#' plot(argos10, which="TS")}
#'
#' # Example 4: map with bathymetry
#' # (Slow, so not run by default.)
#'\dontrun{
#' par(mar=c(3, 3, 1, 1))
#' plot(index, bathymetry=TRUE)}
#'
#' # Example 5: map with user-downloaded bathymetry
#' # (Slow, so not run by default.)
#'\dontrun{
#' par(mar=c(3, 3, 1, 1))
#' # Download a bathymetry that spans a wider range than the data, at
#' # 1-minute (approximately 2km) resolution. Consider using save()
#' # to save 'bathy' to an .rda file, and load() to reload it.
#' bathy <- marmap::getNOAA.bathy(-82, -71, 23, 30, 1)
#' # Adjust colormap to show water depth, not elevation above sea level.
#' cm <- colormap(zlim=c(0, -min(bathy)), col=function(...) rev(oceColorsGebco(...)))
#' plot(index, bathymetry=bathy, colormap=cm)}
#'
#' @importFrom grDevices gray rgb
#' @importFrom graphics par points polygon
#' @importFrom utils data
#' @importFrom oce as.ctd colormap drawPalette imagep oceColorsGebco plotTS
#' @importFrom marmap getNOAA.bathy
#' @export
#' @aliases plot,argoFloats-method
#' @author Dan Kelley
setMethod(f="plot",
          signature=signature("argoFloats"),
          definition=function(x,
                              which="map",
                              bathymetry=FALSE,
                              colormap=NULL,
                              xlab=NULL, ylab=NULL,
                              cex=NULL, col=NULL, pch=NULL, bg=NULL,
                              mar=NULL, mgp=NULL,
                              eos="gsw",
                              debug=0,
                              ...)
          {
              debug <- if (debug > 2) 2 else max(0, floor(debug + 0.5))
              argoFloatsDebug(debug, "plot(x, which=\"", which, "\") {\n", sep="", unindent=1)
              if (!inherits(x, "argoFloats"))
                  stop("method is only for objects of class 'argoFloats'")
              omgp <- par("mgp")
              if (is.null(mgp))
                  mgp <- c(2, 0.7, 0)
              omar <- par("mar")
              if (is.null(mar))
                  mar <- par("mar") #c(mgp[1] + 1.5, mgp[1] + 1.5, mgp[1], mgp[1])
              par(mar=mar, mgp=mgp)
              if (which == "map") {
                  longitude <- x[["longitude", debug=debug]]
                  latitude <- x[["latitude", debug=debug]]
                  drawBathymetry <- inherits(bathymetry, "bathy") || (is.logical(bathymetry) && bathymetry)
                  argoFloatsDebug(debug, "drawBathymetry calculated to be", drawBathymetry, "\n")
                  if (drawBathymetry) {
                      if (is.logical(bathymetry)) {
                          ## We must download. We use a slightly larger area, which
                          ## may prevent white-space around the image, and we set
                          ## a resolution that depends on the latitude range.
                          Dlat <- diff(range(latitude, na.rm=TRUE)) / 2
                          Dlon <- diff(range(longitude, na.rm=TRUE)) / 2
                          resolution <- ifelse(Dlat < 5, 1, ifelse(Dlat < 20, 4, 60))
                          argoFloatsDebug(debug, "downloading bathymetry with resolution=", resolution, "\n")
                          bathymetry <- marmap::getNOAA.bathy(min(longitude, na.rm=TRUE)-Dlon,
                                                              max(longitude, na.rm=TRUE)+Dlon,
                                                              min(latitude, na.rm=TRUE)-Dlat,
                                                              max(latitude, na.rm=TRUE)+Dlat,
                                                              resolution)
                      }
                      if (is.null(bathymetry)) {
                          warning("could not download bathymetry from NOAA server\n")
                          drawBathymetry <- FALSE
                      } else {
                          if (!inherits(bathymetry, "bathy"))
                              stop("'bathymetry' must be an object created by marmap::getNOAA.bathy()")
                          if (is.null(colormap)) {
                              argoFloatsDebug(debug, "using a default colormap\n")
                              colormap <- oce::colormap(zlim=c(0, -min(bathymetry)),
                                                        col=function(...) rev(oceColorsGebco(...)))
                          }
                          argoFloatsDebug(debug, "drawing bathymetry palette\n")
                          drawPalette(colormap=colormap)
                      }
                  }
                  argoFloatsDebug(debug, "map plot\n", sep="")
                  if (is.null(cex))
                      cex <- 1
                  if (is.null(col))
                      col <- "white"
                  if (is.null(pch))
                      pch <- 21
                  if (is.null(bg))
                      bg <- "red"
                  xlab <- if (is.null(xlab)) "Longitude" else xlab
                  ylab <- if (is.null(ylab)) "Latitude" else ylab
                  plot(range(longitude), range(latitude),
                       asp=1/cos(pi/180*mean(range(latitude, na.rm=TRUE))),
                       xlab=xlab, ylab=ylab, type="n")
                  if (drawBathymetry)
                      imagep(as.numeric(rownames(bathymetry)),
                             as.numeric(colnames(bathymetry)),
                             -bathymetry, colormap=colormap, add=TRUE)
                  points(longitude, latitude, cex=cex, col=col, pch=pch, bg=bg, ...)
                  if (requireNamespace("ocedata", quietly=TRUE)) {
                      data("coastlineWorldFine", package="ocedata", envir=environment())
                      coastlineWorldFine <- get("coastlineWorldFine")
                      polygon(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]],
                              col="tan")
                  } else if (requireNamespace("oce", quietly=TRUE)) {
                      data("coastlineWorld", package="ocedata", envir=environment())
                      coastlineWorld <- get("coastlineWorld")
                      polygon(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]],
                              col="tan")
                  }
                  par(mar=omar, mgp=omgp)
              } else if (which == "TS") {
                  argoFloatsDebug(debug, "TS plot\n", sep="")
                  if ((x[["type"]] != "argos"))
                      stop("In plot() : x must have been created by readProfiles()", call.=FALSE)
                  if (!(eos %in% c("gsw", "unesco")))
                      stop("eos must be \"gsw\" or \"unesco\", not \"", eos, "\"")
                  salinity <- unlist(x[["salinity", debug=debug]])
                  temperature <- unlist(x[["temperature", debug=debug]])
                  pressure <- unlist(x[["pressure", debug=debug]])
                  latitude <- unlist(x[["latitude", debug=debug]])
                  longitude <- unlist(x[["longitude", debug=debug]])
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
              } else {
                  stop("cannot handle which=\"", which, "\"; try \"map\".")
              }
              argoFloatsDebug(debug, "} # plot()\n", sep="", unindent=1)
          }
)
