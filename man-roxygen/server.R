#' @param server character value, or vector of character values, giving
#' the base name(s) of server(s) holding argo profile files.  These servers
#' are tried sequentially until one of them works.  The default
#' value of `server` is `"auto"`, which is automatically
#' expanded to
#' `c("ftp://usgodae.org/pub/outgoing/argo", "ftp://ftp.ifremer.fr/ifremer/argo")`,
#' meaning to try the USGODAE server first, but to switch to the IFREMER
#' server if that fails.

