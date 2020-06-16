## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Merge argoFloats Indices
#'
#' @param x,y two [`argoFloats-class`] objects of type `index`, e.g. as created by [getIndex()].
#'
#' @param ... optional additional objects like `x` and `y`.
#'
#' @return An [`argoFloats-class`] object of type `index`.
#'
#' @examples
#' library(argoFloats)
#' data(index)
#'
#' # Index of floats within 50km of Abaca Island
#' C <- subset(index, circle=list(longitude=-77.5, latitude=27.5, radius=50))
#'
#' # Index of floats within a rectangle near Abaca Island
#' lonRect <- c(-76.5, -76)
#' latRect <- c(26.5, 27.5)
#' R <- subset(index, rectangle=list(longitude=lonRect, latitude=latRect))
#'
#' RC <- merge(C, R)
#'\dontrun{
#' plot(RC, bathymetry=FALSE)}
#'
#' @author Dan Kelley
#'
#' @export
#' @aliases merge,argoFloats-method
setMethod(f="merge",
          signature="argoFloats",
          definition=function(x, y, ...) {
              if (!requireNamespace("oce", quietly=TRUE))
                  stop("must install.packages(\"oce\") for merge() to work")
              dots <- list(...)
              if (!inherits(x, "argoFloats"))
                  stop("'x' must be an argoFloats object")
              if ("index" != x[["type"]])
                  stop("'x' was not created with getIndex().")
              destdir <- x[["destdir"]]
              ftpRoot <- x[["ftpRoot"]]
              if (!inherits(y, "argoFloats"))
                  stop("'y' must be an argoFloats object")
              if ("index" != y[["type"]])
                  stop("'y' was not created with getIndex().")
              if (destdir != y[["destdir"]])
                  stop("'x' and 'xy' must have the same destdir.  Use same 'destdir' in getIndex() call.")
              if (!all.equal(ftpRoot, y[["ftpRoot"]]))
                  stop("'x' and 'xy' must have the same ftpRoot. Use same 'server' in getIndex() call.")
              for (i in seq_along(dots)) {
                  message('i=',i)
                  if (!inherits(dots[[i]], "argoFloats"))
                      stop("argument ", i+2, " is not an argoFloats object")
                  if ("index" != dots[[i]][["type"]])
                      stop("argument ", i+2, " was not created with getIndex().")
                  if (destdir != dots[[i]][["destdir"]])
                      stop("argument ", i+2, " does not have same destdir as previous arguments. Use same 'destdir' in getIndex() call.")
                  if (ftpRoot != dots[[i]][["ftpRoot"]])
                      stop("argument ", i+2, " does not have same ftpRoot as previous arguments.  Use same 'server' in getIndex() call.")
              }
              ## OK, all data are okay, so we can construct the return value
              res <- x
              res@data$index <- rbind(x@data$index, y@data$index)
              for (i in seq_along(dots)) {
                  res@data$index <- rbind(x@data$index, dots[[i]]@data$index)
              }
              res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
              res
          }
)


