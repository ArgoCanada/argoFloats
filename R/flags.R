## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

## ## @import oce
## #' @importFrom oce presentTime
## setGeneric("handleFlags", getGeneric("handleFlags", package="oce"))


#' Handle flags in an argoFloats object of type "argos"
#'
#' The work is done by using [oce::handleFlags,argo-method()] on each of the
#' argo profiles that are stored in the `argos` item within the `data` slot
#' of `object`.  Apart from `object`, all arguments are passed to
#' [oce::handleFlags,argo-method()].
#'
#' @param object A [argoFloats-class] object of type `"argos"`, as created by
#' [readProfiles()].
#'
#' @param flags A vector of integers that are flag values for data that are
#' to be set to `NA` in the returned result.  The default, `NULL`, means to
#' use the flags set up by [readProfiles()].
#'
#' @param actions The actions to perform. The default, `NULL`, means to
#' use the actions set up by [readProfiles()].
#'
#' @param where Passed to [oce::handleFlags,argo-method()].
#'
#' @param debug Passed to [oce::handleFlags,argo-method()].
#'
#' @return A copy of `object` but with each argo object contained therein having
#' been passed through [oce::handleFlags,argo-method()].
#'
#' @export
#'
#' @examples
#' library(argoFloats)
#' # Contrast TS diagrams for raw and flag-handled data
#' data(index)
#' i <- subset(index, 1:5) # first 5 profiles
#' a <- readProfiles(getProfiles(i))
#' A <- handleFlags(a)
#' par(mfrow=c(1, 2))
#' plot(a, which="TS")
#' plot(A, which="TS")
#'
#' @author Dan Kelley
setGeneric("handleFlags", function(object, flags=NULL, actions=NULL, where=NULL, debug=getOption("oceDebug")) {
           standardGeneric("handleFlags")
         })

#' Handle flags in an argoFloats object of type "argos"
#'
#' The work is done by using [oce::handleFlags,argo-method()] on each of the
#' argo profiles that are stored in the `argos` item within the `data` slot
#' of `object`.  Apart from `object`, all arguments are passed to
#' [oce::handleFlags,argo-method()].
#'
#' @param object A [argoFloats-class] object of type `"argos"`, as created by
#' [readProfiles()].
#'
#' @param flags A vector of integers that are flag values for data that are
#' to be set to `NA` in the returned result.  The default, `NULL`, means to
#' use the flags set up by [readProfiles()].
#'
#' @param actions The actions to perform. The default, `NULL`, means to
#' use the actions set up by [readProfiles()].
#'
#' @param where Passed to [oce::handleFlags,argo-method()].
#'
#' @param debug Passed to [oce::handleFlags,argo-method()].
#'
#' @return A copy of `object` but with each argo object contained therein having
#' been passed through [oce::handleFlags,argo-method()].
#'
#' @export
#'
#' @examples
#' library(argoFloats)
#' # Contrast TS diagrams for raw and flag-handled data
#' data(index)
#' i <- subset(index, 1:5) # first 5 profiles
#' a <- readProfiles(getProfiles(i))
#' A <- handleFlags(a)
#' par(mfrow=c(1, 2))
#' plot(a, which="TS")
#' plot(A, which="TS")
#'
#' @author Dan Kelley
setMethod("handleFlags", signature=c(object="argoFloats", flags="ANY", actions="ANY", where="ANY", debug="ANY"),
          definition=function(object, flags=NULL, actions=NULL, where=NULL, debug=0) {
              if ("argos" != object[["type"]])
                  stop("can only handle flags in an object of type \"argos\", as created with readProfiles()")
              res <- object
              res@data$argos <- lapply(object@data$argos, oce::handleFlags, flags=flags, actions=actions, where=NULL, debug=debug)
              res
          })
