% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extdata.R
\docType{data}
\encoding{UTF-8}
\name{D4900785_048.nc}
\alias{D4900785_048.nc}
\title{Sample argo file (delayed data)}
\description{
This is netcdf file for cycle 048 of Argo float 4900785, downloaded from
\url{ftp://usgodae.org/pub/outgoing/argo/dac/aoml/4900785/profiles/D4900785_048.nc}
on 2020 June 24.
As its filename indicates, it holds "realtime" data.
}
\examples{
library(argoFloats)
a <- readProfiles(system.file("extdata", "D4900785_048.nc", package="argoFloats"))
summary(a)
summary(a[[1]])

}
\seealso{
Other raw datasets: 
\code{\link{SD5903586_001.nc}},
\code{\link{SR2902204_131.nc}}
}
\concept{raw datasets}