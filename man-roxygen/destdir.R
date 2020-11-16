#' @param destdir character value indicating the directory in which to store
#' downloaded files. The default value is to compute this using
#' [argoDefaultDestdir()], which returns `~/data/argo` by default,
#' although it also provides ways to set other values using
#' [options()].
#' Set `destdir=NULL`
#' if `destfile` is a filename with full path information.
#' File clutter is reduced by creating a top-level directory called
#' `data`, with subdirectories for various file types; see
#' \dQuote{Examples}.

