#' Show information about argoFloats object
#'
#' @param object an [argoFloats-class] object.
#'
#' @examples
#' library(argoFloats)
#' data(index)
#' index
#'
#' @importFrom methods show
#' @export
#' @docType methods
#' @rdname argoFloats-methods
#'
#' @author Jaimie Harbin
setMethod(f="show",
          signature="argoFloats",
          definition=function(object) {
              type <- object[['type']]
              if (type == "index") {
                  cat('argoFloats object of type "',
                      object[['type']],
                      '" with ',
                      dim(object[['index']])[1],
                      " items\n",
                      sep="")
              } else if (type == "profiles") {
                  cat('argoFloats object of type "',
                      object[['type']],
                      '" with ',
                      length(object@data$file),
                      " items\n",
                      sep="")
              } else if (type == "argos") {
                  cat('argoFloats object of type "',
                      object[['type']],
                      '" with ',
                      length(object@data$argos),
                      " items\n",
                      sep="")
              } else stop("Unknown type =", type)
          })
