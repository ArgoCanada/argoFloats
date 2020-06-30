#' Sample argo file (synthetic realtime data)
#'
#' This is the netcdf file for profile 131 of argo float 2902204, downloaded from
#' \url{ftp://ftp.ifremer.fr/ifremer/argo/dac/incois/2902204/profiles/SR2902204_131.nc}
#' on 2020 June 24.
#' As its filename indicates, it holds the "synthetic" version of "realtime" data.
#'
#' @name SR2902204_131.nc
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' library(argoFloats)
#' a <- readProfiles(system.file("extdata", "SR2902204_131.nc", package="argoFloats"))
#' summary(a)
#' summary(a[[1]])
#'
#' @family raw datasets
NULL

#' Sample argo file (delayed data)
#'
#' This is netcdf file for profile 048 of argo float 4900785, downloaded from
#' \url{ftp://usgodae.org/pub/outgoing/argo/dac/aoml/4900785/profiles/D4900785_048.nc}
#' on 2020 June 24.
#' As its filename indicates, it holds "realtime" data.
#'
#' @name D4900785_048.nc
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' library(argoFloats)
#' a <- readProfiles(system.file("extdata", "D4900785_048.nc", package="argoFloats"))
#' summary(a)
#' summary(a[[1]])
#'
#' @family raw datasets
NULL


#' Sample argo file (delayed synthetic bgc data)
#'
#' This is the netcf file for profile 001 of argo float SD5903586, downloaded
#' \url{ftp://usgodae.org/pub/outgoing/argo/dac/aoml/5903586/profiles/SD5903586_001.nc}
#' on 2020 June 24.
#' As its filename indicates, it holds "synthetic" data
#' in "delayed" mode. The oxygen values are adjusted by 16%, as
#' is shown here, and in the documentation for [useAdjusted()].
#'
#' @name SD5903586_001.nc
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' library(argoFloats)
#' a <- readProfiles(system.file("extdata", "SD5903586_001.nc", package="argoFloats"))
#' a1 <- a[[1]]
#' # Illustrate oxygen adjustment
#' p <- a1[["pressure"]]
#' O <- a1[["oxygen"]]
#' Oa  <- a1[["oxygenAdjusted"]]
#' Percent <- 100 * (Oa - O) / O
#' hist(Percent, main="Oxygen adjustment")
#'
#' @family raw datasets
NULL

