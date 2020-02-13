#' @param mode character value passed to [curl::curl_download()],
#' controlling the mode used in writing the local file. The default
#' value, `mode="wb"`, means to set the write format to binary. This
#' is necessary on Windows machines, which otherwise will alter newline
#' characters, ruining file integrity for binary files (such as
#' netcdf files). The setting is not required on other machines, but
#' it makes sense to use this default in all instances.  See the
#' documentation for [curl::curl_download()].

