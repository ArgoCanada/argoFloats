#' Plot an argoFloats object
#'
#' The action depends on the `type` of the object, and
#' this is set up by the function that created the object;
#' see \dQuote{Details}.
#'
#' The various plot types are as follows.
#' * for `which="map"`, a map of profile locations is created. This
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
#' @param x an [argoFloats-class] object.
#' @param which a string that indicates the type of plot; see \dQuote{Details}.
#' @param cex character expansion factor for plot symbols, or `NULL`, to get an
#' value that depends on the value of `which`.
#' @param col colour to be used for plot symbols, or `NULL`, to get an
#' value that depends on the value of `which`.
#' @param pch number indicating the type of plot symbol, or `NULL`, to get an
#' value that depends on the value of `which`.
#' @param mar either a four-element vector giving the `par("mar")` values to
#' be used for the plot, or `NULL`, which means that this function will determine
#' the margin based on the plot type.
#' @param mgp similar to `mar`, but for `par("mgp")`.
#' @param debug an integer specifying the level of debugging.
#' @param \dots extra arguments passed to the plot calls that are made
#' to within this function. A common use for `which="map"` cases is
#' to set `xlim` and `ylim` to get enlarge the plot region, so a coastline
#' becomes visible.
#'
#' @examples
#' # Example 1
#' library(argoFloats)
#' data(index)
#' plot(index, which="map")
#'
#' @importFrom grDevices rgb
#' @importFrom graphics par polygon
#' @importFrom utils data
#' @export
#' @aliases plot,argoFloat-method
#' @author Dan Kelley
setMethod(f="plot",
          signature=signature("argoFloats"),
          definition=function(x,
                              which="map",
                              cex=NULL,
                              col=NULL,
                              pch=NULL,
                              mar=NULL, mgp=NULL,
                              debug=0,
                              ...)
          {
              if (!inherits(x, "argoFloats"))
                  stop("method is only for objects of class 'argoFloats'")
              if (which == "map") {
                  longitude <- x[["longitude"]]
                  latitude <- x[["latitude"]]
                  if (is.null(cex))
                      cex <- 1
                  if (is.null(col))
                      col <- rgb(0, 0, 1, 0.25)
                  if (is.null(pch))
                      pch <- 20
                  omar <- par("mar")
                  if (is.null(mar))
                      mar <- c(2, 2, 1, 1)
                  omgp <- par("mgp")
                  if (is.null(mgp))
                      mgp <- c(2, 0.7, 0)
                  par(mar=mar, mgp=mgp)
                  plot(longitude, latitude, asp=1/cos(pi/180*mean(range(latitude, na.rm=TRUE))),
                       xlab="", ylab="",
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
              } else {
                  stop("cannot handle which=\"", which, "\"; try \"map\".")
              }
          }
)
