## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Get the default Argo cache directory
#'
#' By default, files downloaded from the Argo mirror are placed in
#' `~/data/argo`. You can configure where these files are stored by setting
#' `options(argoFloat.destdir="path/to/argo")`. To persist this value
#' between sessions you can place this value in your `.Rprofile`.
#'
#' @return A character value containing a path to the default Argo data directory.
#' @export
#'
#' @examples
#' argoDefaultDestdir()
#'
argoDefaultDestdir <- function()
{
    argoOptionValue("argoFloats.destdir", "R_ARGOFLOATS_DESTDIR", "~/data/argo")
}

#' @rdname argoDefaultDestdir
#' @export
argoDefaultIndexAge <- function()
{
    as.numeric(argoOptionValue("argoFloats.indexAge", "R_ARGOFLOATS_INDEX_AGE", 1))
}

#' @rdname argoDefaultDestdir
#' @export
argoDefaultProfileAge <- function()
{
    as.numeric(argoOptionValue("argoFloats.profileAge", "R_ARGOFLOATS_PROFILE_AGE", 365))
}

#' @rdname argoDefaultDestdir
#' @export
hasArgoTestCache <- function() 
{
    !is.null(getOption("argoFloats.destdir")) || (Sys.getenv("R_ARGOFLOATS_DESTDIR") != "")
}

argoOptionValue <- function(option, envVar, default)
{
    value <- getOption(option, default = NULL)
    if (!is.null(value))
        return(value)
    ## Setting by environment variable is useful on CI, where multiple
    ## R sessions may be started and should all have the same value
    value <- Sys.getenv(envVar, unset = "")
    if (value != "")
        return(value)
    default
}
