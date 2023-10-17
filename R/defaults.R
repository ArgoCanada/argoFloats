## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Get Default Values
#'
#' These are helper functions that permit customization of various aspects of
#' functions within the argoFloats package. The idea is that values can be set
#' using [options()] or by using system 'environment variables', freeing the
#' user from the necessity of altering the parameters provided to various
#' argoFloats functions.  See \dQuote{Details} for more on the individual
#' functions, noting that the entry for `argoDefaultServer()` is written with
#' the most detail, with other entries relying on the background
#' established there.
#'
#' * `argoDefaultServer()`
#' The [getIndex()] and [getProfiles()] functions download data from a
#' remote machine with URL specified by an argument named `server`. A user may
#' prefer one server over another, perhaps due to speed of downloads to
#' a particular research laboratory.  However, that choice might not be best
#' for another user, or even the same user at another time.  Code reusability
#' would be enhanced if the user had a way to alter the value of the `server`
#' argument across all code, thereby eliminating the need to work in a text
#' editor to find all instances of the function call.  This is where
#' `argoDefaultServer()` is useful.  It lets the user specify a value for
#' `server` either in R, using a call like
#' `options(argoFloats.server="ifremer-https")`
#' within R code (perhaps in the user's `.Rprofile` file),
#' or by defining an environment variable named
#' `R_ARGOFLOATS_SERVER` at the operating-system level.  If
#' the `argoFloats.server` option has not been set in R,
#' and `R_ARGOFLOATS_SERVER` has not been set in the OS, then
#' `argoDefaultServer()` defaults to `c("ifremer-https","usgodae")`.
#'
#' * `argoDefaultDestdir()` returns the name of the local directory
#' into which to store indices and other argo data.
#' The option is named
#' `argoFloats.destdir`,
#' the environment variable is named
#' `R_ARGOFLOATS_DESTDIR`,
#' and the default is
#' `"~/data/argo".
#'
#' * `argoDefaultIndexAge()` returns the number of days beyond
#' which an index is regarded as stale (and thus in need of a new
#' download).
#' The option is named
#' `argoFloats.indexAge`,
#' the environment variable is named
#' `R_ARGOFLOATS_INDEX_AGE`,
#' and the default is
#' 1.0, for 1 day.
#'
#' * `argoDefaultProfileAge()` returns the number of days beyond
#' which an individual profile netCDF file is regarded as stale (and thus in need of a new
#' download).
#' The option is named
#' `argoFloats.profileAge`,
#' the environment variable is named
#' `R_ARGOFLOATS_PROFILE_AGE`,
#' and the default is
#' 365.0 days.  (Note that this is much higher than the value for
#' `argoDefaultIndexAge()`, on the assumption that users will prefer
#' recent indices, to get new data, but will prefer to update
#' profile-specific datasets infrequently.)
#'
#' * `argoDefaultBathymetry()` returns a value for the `bathymetry`
#' argument used by [plot,argoFloats-method()].
#' The option is named
#' `argoFloats.bathymetry`,
#' the environment variable is named
#' `R_ARGOFLOATS_BATHYMETRY`,
#' and the default is
#' `FALSE`.
#'
#' * `hasArgoTestCache()` is not a user-facing function. Rather, its purpose
#' is to speed the running of test suites during development, by preventing
#' multiple downloads of data already downloaded.
#'
#' @return A value as described above, depending on the particular
#' function in question.
#'
#' @export
#'
#' @examples
#' argoDefaultServer()
#' argoDefaultDestdir()
#' argoDefaultIndexAge()
#' argoDefaultProfileAge()
#' argoDefaultBathymetry()
#'
argoDefaultDestdir <- function()
{
    argoOptionValue("argoFloats.destdir", "R_ARGOFLOATS_DESTDIR", "~/data/argo")
}

#' @rdname argoDefaultDestdir
#' @export
argoDefaultServer <- function()
{
    argoOptionValue("argoFloats.server", "R_ARGOFLOATS_SERVER", c("ifremer-https", "usgodae"))
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
argoDefaultBathymetry <- function()
{
    !is.null(getOption("argoFloats.bathymetry")) || (Sys.getenv("R_ARGOFLOATS_BATHYMETRY") != "")
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
