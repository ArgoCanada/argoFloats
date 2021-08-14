#' @param skip A logical value indicating whether to skip over netcdf
#' files that cannot be downloaded from the server.  This is `FALSE` by
#' default, so that [getProfiles()] will raise an error if it is
#' impossible to re-download an outdated file.  This is not
#' unexpected with built-in index files, e.g. `data(index)`,
#' because this package cannot be updated every time the Argo
#' servers change the names of netcdf files.  However, users
#' are commonly working with index files they created with
#' [getIndex()], so they ought to be up-to-date.


