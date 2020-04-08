## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Summarize an argoFloats Object
#'
#' Show some key facts about an [argoFloats-class] object.
#'
#' @param object an [argoFloats-class] object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @examples
#' library(argoFloats)
#' data(index)
#' summary(index)
#'
#' @importFrom oce processingLogShow
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
                  with(object@metadata, {
                      cat("* server:      ", server, "\n", sep="")
                      cat("* file:        ", file, "\n", sep="")
                      cat("* url:         ", url, "\n", sep="")
                      cat("* ftpRoot:     ", ftpRoot, "\n", sep="")
                      cat("* destfileRda: ", destfileRda, "\n", sep="")
                      if (length(header)) {
                          cat("* header:\n", sep="")
                          for (h in header)
                              cat("    ", h, "'\n", sep="")
                      } else {
                          cat("* header: (none)\n", sep="")
                      }})
                  ndata <- length(object@data$index$file)
                  if (ndata > 0) {
                      cat("* index (each holding ", ndata, if (ndata>1) " elements):\n" else " element):\n", sep="")
                      for (name in names(object@data$index)) {
                          cat("    ", name, ": ",
                              paste(head(object@data$index[[name]], 3), collapse=", "),
                              if (ndata > 1) ", ...\n" else "\n", sep="")
                      }
                  } else {
                      cat("* index: (none)\n", sep="")
                  }
              } else if (object@metadata$type == "profiles") {
                  cat("* FIXME: add special info for 'profiles' type\n")
              }
              processingLogShow(object)
              invisible()
          }
)

