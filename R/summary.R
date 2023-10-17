# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

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
#' @importFrom utils head
#'
#' @export
#'
#' @return None (invisible NULL).
#'
#' @author Dan Kelley
setMethod(f="summary",
    signature="argoFloats",
    definition=function(object, ...)
    {
        if (!requireNamespace("oce", quietly=TRUE)) {
            stop("must install.packages(\"oce\") for summary() to work")
        }
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
                for (h in object@metadata$header) {
                    cat("    ", h, "\n", sep="")
                }
            } else {
                cat("* header: (none)\n", sep="")
            }
            ndata <- length(object@data$index$file)
            if (ndata > 0) {
                colnames <- names(object@data$index)
                cat("* index with ", ndata,
                    if (ndata>1) " rows " else " row ",
                    "and column names: \"",
                    paste(head(colnames, -1), collapse="\", \""),
                    "\" and \"", tail(colnames, 1), "\"\n", sep="")
            } else {
                cat("* index: (none)\n", sep="")
            }
            cat("* hint: use getProfiles() to download these files from the repository\n")
        } else if (object@metadata$type == "profiles") {
            file <- object@data$file
            cat("* files:       \"", file[1], "\" (+", length(file) - 1L, " others)\n", sep="")
            #cat("* hint:        use readProfiles() on this to read these files\n")
        } else if (object@metadata$type == "argos") {
            cat("* contains", length(object@data$argos), "objects\n")
            #cat("* hint: access list of oce::argo objects with x[[\"argos\"]] or a single one with e.g. x[[1]]\n")
        }
        oce::processingLogShow(object)
        invisible(NULL)
    })
