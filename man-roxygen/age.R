#' @param age a numerical value indicating a time interval, in days.  If the file
#' to be downloaded from the server already exists locally, and was created
#' is less than `age` days in the past, it will not be downloaded.  The default,
#' [argoDefaultProfileAge()], is one year.  Setting `age=0` will force a download.
