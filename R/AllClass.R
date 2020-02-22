#' A Package for Collections of Argo Float Profiles
#'
#' This package **FIXME(jh) Write a few paragraphs here, after
#' discussions with dk and cr.  The idea will be to get a clean
#' introductory paragraph that will organize our intentions for
#' the object structure.  Details ought to be added later, as
#' new things get written.  The oce package may provide a useful
#' guide.**

#' @importFrom methods new
#' @import knitr
#' @importFrom oce subset summary vectorShow
#' @docType package
NULL

#'
#' Class to hold argoFloats objects
argoFloats <- setClass("argoFloats", contains="oce")


setMethod(f="initialize",
          signature="argoFloats",
          definition=function(.Object, type="unspecified") {
              .Object@metadata$type <- type
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'argoFloats' object"
              return(.Object)
          })

#' Extract Something From an argoFloats Object
#'
#' @param x a [argoFloats-class] object.
#' @param i a character value specifying an item within the object's `data` slot. Partial
#' matches are allowed, e.g. `x[["lon"]]` functions the same as `x[["lon"]]`, because only
#' item matches to those three letters.
#' @param j ignored.
#' @param ... ignored.
#'
#' @author Dan Kelley
#'
#' @export
setMethod(f="[[",
          signature(x="argoFloats", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (missing(i))
                  stop("Must name an item to retrieve, e.g. 'x[[\"latitude\"]]'", call.=FALSE)
              if (x@metadata$type == "index") {
                  names <- names(x@data$index)
                  w <- pmatch(i, names)
                  if (is.finite(w))
                      return(x@data$index[[names[w]]])
                  else
                      stop("Unknown item '", i, "'; must be one of: '", paste(names, collapse="', '"), "'", call.=FALSE)
              } else {
                  stop("only for type 'index'")
              }
          })




#' Summarize an argoFloats Object
#'
#' @param object a [argoFloats-class] object.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom oce processingLogShow vectorShow
#' @importFrom methods callNextMethod
#' @importFrom utils head
#'
#' @export
#'
#' @aliases summary.argoFloats
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
                      cat("* index (each holding ", ndata, " elements):\n", sep="")
                      for (name in names(object@data$index)) {
                          cat("    ", name, ": ", paste(head(object@data$index[[name]], 3), collapse=", "), ", ...\n", sep="")
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


