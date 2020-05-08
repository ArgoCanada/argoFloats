## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Summarize an argoFloats Object
#'
#' Show some key facts about an [`argoFloats-class`] object.
#'
#' @param object an [`argoFloats-class`] object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @examples
#' library(argoFloats)
#' data(index)
#' summary(index)
#'
#' @importFrom oce processingLogShow vectorShow
#' @importFrom methods callNextMethod
#' @importFrom utils head
#' @export
#'
#' @author Dan Kelley
setMethod(f="summary",
          signature="argoFloats",
          definition=function(object, ...)
          {
              cat("argoFloats summary\n------------------\n\n")
              cat("* type:        ", object@metadata$type, "\n", sep="")
              if (object@metadata$type == "index") {
                  if (length(object@metadata$server) > 1) {
                      cat("* server:      c(\"", paste(object@metadata$server, collapse="\",\n                 \""), "\")\n", sep="")
                  } else {
                      cat("* server:      \"", object@metadata$server, "\"\n", sep="")
                  }
                  cat("* file:        ", object@metadata$file, "\n", sep="")
                  cat("* url:         ", object@metadata$url, "\n", sep="")
                  if (length(object@metadata$ftpRoot) > 1) {
                      cat("* ftpRoot:     c(\"", paste(object@metadata$ftpRoot, collapse="\",\n                 \""), "\")\n", sep="")
                  } else {
                      cat("* ftpRoot:      \"", object@metadata$ftpRoot, "\"\n", sep="")
                  }
                  cat("* destfileRda: ", object@metadata$destfileRda, "\n", sep="")
                  if (length(object@metadata$header)) {
                      cat("* header:\n", sep="")
                      for (h in object@metadata$header)
                          cat("    ", h, "'\n", sep="")
                  } else {
                      cat("* header: (none)\n", sep="")
                  }
                  ndata <- length(object@data$index$file)
                  if (ndata > 0) {
                      cat("* index (each item holding ", ndata, if (ndata>1) " elements):\n" else " element):\n", sep="")
                      for (name in names(object@data$index)) {
                          cat(oce::vectorShow(name))
                          ## cat("    ", name, ": ",
                          ##     paste(head(object@data$index[[name]], 3), collapse=", "),
                          ##     if (ndata > 1) ", ...\n" else "\n", sep="")
                      }
                  } else {
                      cat("* index: (none)\n", sep="")
                  }
              } else if (object@metadata$type == "profiles") {
                  cat("* FIXME: add special info for 'profiles' type\n")
              }
              oce::processingLogShow(object)
              invisible()
          })

