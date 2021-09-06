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
#'\donttest{
#' plot(RC, bathymetry=FALSE)
#'}
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
                  stop("in merge,argoFloats-method():\n 'x' must be an argoFloats object")
              if ("index" != x[["type"]])
                  stop("in merge,argoFloats-method():\n 'x' was not created with getIndex().", call.=FALSE)
              ftpRoot <- x[["ftpRoot"]]
              if (!inherits(y, "argoFloats"))
                  stop("in merge,argoFloats-method():\n 'y' must be an argoFloats object", call.=FALSE)
              if ("index" != y[["type"]])
                  stop("in merge,argoFloats-method():\n 'y' was not created with getIndex().", call.=FALSE)
              if (!all.equal(ftpRoot, y[["ftpRoot"]]))
                  stop("in merge,argoFloats-method():\n 'x' and 'xy' must have the same ftpRoot. Use same 'server' in getIndex() call.", call.=FALSE)
              for (i in seq_along(dots)) {
                  if (!inherits(dots[[i]], "argoFloats"))
                      stop("in merge,argoFloats-method():\n argument ", i+2, " is not an argoFloats object", call.=FALSE)
                  if ("index" != dots[[i]][["type"]])
                      stop("in merge,argoFloats-method():\n argument ", i+2, " was not created with getIndex().",call.=FALSE)
                  if (ftpRoot != dots[[i]][["ftpRoot"]])
                      stop("in merge,argoFloats-method():\n argument ", i+2, " does not have same ftpRoot as previous arguments.  Use same 'server' in getIndex() call.",call.=FALSE)
              }
              ## OK, all data are okay, so we can construct the return value
              res <- x
              ## Ensure that we have the two extra columns that Bgc data have, viz.
              ## "parameters" and "parameter_data_mode"
              X <- x@data$index
              if (!"parameters" %in% names(X))
                  X$parameters <- NA
              if (!"parameter_data_mode" %in% names(X))
                  X$parameter_data_mode <- NA
              Y <- y@data$index
              if (!"parameters" %in% names(Y))
                  Y$parameters <- NA
              if (!"parameter_data_mode" %in% names(Y))
                  Y$parameter_data_mode <- NA
              INDEX <- rbind(X, Y)
              for (i in seq_along(dots)) {
                  D <- dots[[i]]@data$index
                  if (!"parameters" %in% names(D))
                      D$parameters <- NA
                  if (!"parameter_data_mode" %in% names(D))
                      D$parameter_data_mode <- NA
                  INDEX <- rbind(INDEX, D)
              }
              res@data$index <- INDEX
              res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
              res
          }
)


