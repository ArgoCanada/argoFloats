#' Show Information About argoFloats Object
#'
#' This produces a one-line explanation of the contents of the object,
#' that typically indicates the type and the number of items referenced
#' by the object. Those items depend on the type of object:
#' URLs if `metadata$type` is `"index"`, local filenames if
#' `"profiles"`, or oce-created argo objects if `"argos"`.
#' As with other R `show()` methods, it may be invoked in an interactive
#' session just by typing the object, or by using `print()`; see the Examples.
#'
#' @param object an [`argoFloats-class`] object.
#'
#' @examples
#' library(argoFloats)
#' data(index)
#' show(index)
#' print(index)
#' index
#'
#' @importFrom methods show
#' @export
#' @docType methods
#' @rdname argoFloats-methods
#'
#' @return None (invisible NULL).
#'
#' @author Jaimie Harbin
setMethod(f="show",
          signature="argoFloats",
          definition=function(object) {
              type <- object[["type"]]
              if (type == "index") {
                  cat('argoFloats object of type "',
                      object[["type"]],
                      '" with ',
                      dim(object[["index"]])[1],
                      " items\n",
                      sep="")
              } else if (type == "profiles") {
                  cat('argoFloats object of type "',
                      object[["type"]],
                      '" with ',
                      length(object@data$file),
                      " items\n",
                      sep="")
              } else if (type == "argos") {
                  cat('argoFloats object of type "',
                      object[["type"]],
                      '" with ',
                      length(object@data$argos),
                      " items\n",
                      sep="")
              } else stop("Unknown type =", type)
          })
