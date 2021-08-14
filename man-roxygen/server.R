#' @param server character value, or vector of character values, indicating the name of
#' servers that supply argo data to be acquired with [getIndex()].  If not supplied,
#' the default will be determined with [argoDefaultServer()], which uses
#' a value set by `options("argoFloats.server"=URL)`
#' where `URL` is an appropriate URL, or `"ifremer-https"` if no such option was
#' set.

