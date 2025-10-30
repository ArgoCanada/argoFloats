#' Sample Argo File (Real-Time Synthetic Data)
#'
#' This is the NetCDF file for cycle 131 of Argo float 2902204, downloaded from
#' \code{ftp://ftp.ifremer.fr/ifremer/argo/dac/incois/2902204/profiles/SR2902204_131.nc}
#' on 2020 June 24.
#' As its filename indicates, it holds the "synthetic" version of "real-time" data.
#'
#' @name SR2902204_131.nc
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' library(argoFloats)
#' a <- readProfiles(system.file("extdata", "SR2902204_131.nc", package = "argoFloats"))
#' summary(a)
#' summary(a[[1]])
#'
#' @family raw datasets
NULL

#' Sample Argo File (Delayed Core Data)
#'
#' This is NetCDF file for delayed-mode data for cycle 48 of Argo float 4900785, downloaded from
#' \code{https://data-argo.ifremer.fr/dac/aoml/4900785/profiles/D4900785_048.nc}
#' on 2020 June 24.
#'
#' @name D4900785_048.nc
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' library(argoFloats)
#' a <- readProfiles(system.file("extdata", "D4900785_048.nc", package = "argoFloats"))
#' summary(a)
#' summary(a[[1]])
#'
#' @family raw datasets
NULL

#' Sample Argo File (Real-Time Core Data)
#'
#' This is NetCDF file for real-time data for cycle 163 of Argo float 3901602, downloaded from
#' \code{https://data-argo.ifremer.fr/dac/coriolis/3901602/profiles/R3901602_163.nc}
#' on 2021 March 7.
#'
#' @name R3901602_163.nc
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' library(argoFloats)
#' a <- readProfiles(system.file("extdata", "R3901602_163.nc", package = "argoFloats"))
#' summary(a)
#' summary(a[[1]])
#'
#' @family raw datasets
NULL

#' Sample Argo File (Delayed Synthetic Data)
#'
#' This is the NetCDF file for cycle 1 of Argo float 5903586, downloaded from
#' \code{ftp://usgodae.org/pub/outgoing/argo/dac/aoml/5903586/profiles/SD5903586_001.nc}
#' on 2020 June 24 (this URL appears to be unreliable).
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
#' a <- readProfiles(system.file("extdata", "SD5903586_001.nc", package = "argoFloats"))
#' a1 <- a[[1]]
#' # Illustrate oxygen adjustment
#' p <- a1[["pressure"]]
#' O <- a1[["oxygen"]]
#' Oa <- a1[["oxygenAdjusted"]]
#' Percent <- 100 * (Oa - O) / O
#' hist(Percent, main = "Oxygen adjustment")
#'
#' @family raw datasets
NULL
