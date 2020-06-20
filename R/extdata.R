#' Sample argo file (synthetic realtime data)
#'
#' This is profile 131 for float 2902204, downloaded on June 8,
#' 2020.  As its filename indicates, this is in the "synthetic"
#' data stream of "realtime" data.
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
#' This is profile 048 for float 4900785, downloaded on June 12,
#' 2020.  As its filename indicates, it holds "realtime" data.
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
#' This is profile 001 for float SD5903586, downloaded on June 20,
#' 2020.  As its filename indicates, it holds "synthetic" data
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

