#' @section Caveat about out-of-date index files:
#'
#' Note that the netcdf files on argo repositories are changeable,
#' not just in content, but also in file name.  For example, the data acquired
#' in a given profile of a given float may initially be provided in realtime
#' mode (with a file name containing an "R" as the first or second character),
#' my later be replaced later with a delayed-mode file (with a "D" in the first
#' or second character). Since index files name data files directly, this means
#' that index files can become out-of-date, containing references to netcdf
#' files that no longer exist on the server. This applies to the sample
#' index files provided with this package, and to user files, and it
#' explains why [getProfiles()] skips over files that cannot be downloaded.


