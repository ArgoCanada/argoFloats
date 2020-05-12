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
#' @param which a string that indicates the type of plot; see \dQuote{Details}.
#' @param xlab character value indicating the name for the horizontal axis, or
#' `NULL`, which indicates that this function should choose an appropriate name
#' depending on the value of `which`. Note that `xlab` is not obeyed if
#' `which="TS"`, because altering that label can be confusing to the user.
#' @param ylab as `xlab`, but for th vertical axis.
#' @param cex character expansion factor for plot symbols, or `NULL`, to get an
#' value that depends on the value of `which`.
#' @param col colour to be used for plot symbols, or `NULL`, to get an
#' value that depends on the value of `which`.
#' @param pch number indicating the type of plot symbol, or `NULL`, to get an
#' value that depends on the value of `which`.
#' @param mar either a four-element vector giving the margins to be used for
#' the plot (see [par()] for details), or `NULL`, which means to use
#' [par]`("mar")`.
#' @param mgp either a three-element vector giving the geometry for
#' axis labels (see [par()] for details), or `NULL`, which means to use
#' [par]`("mgp")`.
#' @param eos character value indicating the equation of state to use
#' if `which="TS"`.  This must be `"gsw"` (the default) or `"unesco"`;
#' see [oce::plotTS()].
#' @param debug an integer specifying the level of debugging.
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
#' plot(index, which="map")
#' lon <- index[["longitude"]]
#' lat <- index[["latitude"]]
#' dist <- geodDist(lon, lat, -77.06, 26.54)
#' o <- order(dist)
#' index10 <- subset(index, o[1:10])
#' points(index10[["longitude"]], index10[["latitude"]], col=2)
#'
#' # Example 2: TS of first 10 profiles in the index
#'\dontrun{
#' profiles10 <- getProfiles(index10, destdir="~/data/argo")
#' argos10 <- readProfiles(profiles10)
#' plot(argos10, which="TS")}
#'
#' @importFrom grDevices rgb
#' @importFrom graphics par polygon
#' @importFrom utils data
#' @importFrom oce as.ctd plotTS
#' @export
#' @aliases plot,argoFloats-method
#' @author Dan Kelley
setMethod(f="plot",
          signature=signature("argoFloats"),
          definition=function(x,
                              which="map",
                              xlab=NULL, ylab=NULL,
                              cex=NULL, col=NULL, pch=NULL,
                              mar=NULL, mgp=NULL,
                              eos="gsw",
                              debug=0,
                              ...)
          {
              debug <- if (debug > 2) 2 else max(0, floor(debug + 0.5))
              argoFloatsDebug(debug, "plot(x, which=\"", which, "\") {\n", sep="", unindent=1)
              if (!inherits(x, "argoFloats"))
                  stop("method is only for objects of class 'argoFloats'")
              if (which == "map") {
                  argoFloatsDebug(debug, "map plot\n", sep="")
                  longitude <- x[["longitude", debug=debug]]
                  latitude <- x[["latitude", debug=debug]]
                  if (is.null(cex))
                      cex <- 1
                  if (is.null(col))
                      col <- "gray62"  # col=8 in R-4.x
                  if (is.null(pch))
                      pch <- 20
                  xlab <- if (is.null(xlab)) "Longitude" else xlab
                  ylab <- if (is.null(ylab)) "Latitude" else ylab
                  omgp <- par("mgp")
                  if (is.null(mgp))
                      mgp <- c(2, 0.7, 0)
                  omar <- par("mar")
                  if (is.null(mar))
                      mar <- par("mar") #c(mgp[1] + 1.5, mgp[1] + 1.5, mgp[1], mgp[1])
                  par(mar=mar, mgp=mgp)
                  plot(longitude, latitude, asp=1/cos(pi/180*mean(range(latitude, na.rm=TRUE))),
                       xlab=xlab, ylab=ylab,
                       cex=cex, col=col, pch=pch, ...)
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
