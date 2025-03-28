#' Interactive App For Viewing Argo Float Positions
#'
#' [mapApp()] sets up an interactive "shiny app" session for exploring
#' the spatial-temporal distribution of Argo profiles.  The graphical
#' user interface (GUI) is meant to be reasonably self-explanatory,
#' and a button labelled Help yields a pop-up window with
#' more information that might not be evident at first glance.
#' A number of keystrokes exist to assist the user including: \cr
#' **u**: undo previous actions \cr
#' **i**: zoom in \cr
#' **o**: zoom out \cr
#' **n**: go north \cr
#' **e**: go east \cr
#' **s**: go south \cr
#' **w**: go west \cr
#' **f**: go forward in time \cr
#' **b**: go backward in time \cr
#' **r**: reset to initial state \cr
#' **h**: hold active hover message (press h again to undo) \cr
#' **0**: unzoom an area \cr
#' **d**: toggle debugging flag \cr
#' More details are provided in the rest of this documentation
#' page, and in Section 4 of Kelley et al. (2021).
#'
#' [mapApp()] uses [getIndex()] to download index files from an Argo server
#' the first time it runs.  This can be slow, taking a minute or more, depending
#' on the internet connection and load on the server.  Once the index
#' files are downloaded, [mapApp()] categorizes profiles as Core,
#' BGC, or Deep (which may be displayed in any combination, using
#' GUI elements).
#'
#' The `hi-res` button will only affect the coastline, not the topography,
#' unless there is a local file named `topoWorldFine.rda` that contains
#' an alternative topographic information. Here is how to create such a file:
#' ```R
#' library(oce)
#' topoFile <- download.topo(west=-180, east=180,
#'                           south=-90, north=90,
#'                           resolution=10,
#'                           format="netcdf", destdir=".")
#' topoWorldFine <- read.topo(topoFile)
#' save(topoWorldFine, file="topoWorldFine.rda")
#' unlink(topoFile) # clean up
#' ```
#'
#'
#' @param age numeric value indicating how old a downloaded file
#' must be (in days), for it to be considered out-of-date.  The
#' default, [argoDefaultIndexAge()], limits downloads to once per day, as a way
#' to avoid slowing down a workflow with a download that might take
#' a minute or so. Note that setting `age=0` will force a new
#' download, regardless of the age of the local file.
#'
#' @template server
#'
#' @template destdir
#'
#' @param debug integer value that controls how much information `mapApp()` prints
#' to the console as it works.  The default value of 0 leads to a fairly limited
#' amount of printing, while higher values lead to more information. This information
#' can be helpful in diagnosing problems or bottlenecks.
#'
#' @param colLand a colour specification for the land.
#'
#' @examples
#' if (interactive()) {
#'     library(argoFloats)
#'     mapApp()
#' }
#'
#' @references
#' Kelley, D. E., Harbin, J., & Richards, C. (2021).
#' argoFloats: An R package for analyzing Argo data.
#' Frontiers in Marine Science, (8), 636922.
#' \doi{10.3389/fmars.2021.635922}
#'
#' @return `mapApp` has no return value, since it creates a shiny app by passing
#' control to [shiny::runApp()], which never returns.
#'
#' @author Dan Kelley and Jaimie Harbin
#'
#' @importFrom graphics arrows image lines mtext
#' @importFrom grDevices col2rgb grey
#' @importFrom utils write.table
#'
#' @export
mapApp <- function(
    age = argoDefaultIndexAge(),
    server = argoDefaultServer(),
    destdir = argoDefaultDestdir(),
    colLand = "lightgray",
    debug = 0) {
    debug <- as.integer(max(0, min(debug, 3))) # put in range from 0 to 3
    # Check for related packages and show how to install any that are missing
    need <- c("colourpicker", "curl", "lubridate", "ncdf4", "oce", "ocedata", "s2", "sf")
    need <- need[sapply(need, function(p) !requireNamespace(p, quietly = TRUE))]
    if (length(need) > 0L) {
        stop(
            "please install necessary packages, using\n  install.packages(c(\"",
            paste(need, collapse = "\",\""), "\"))"
        )
    }
    # Establish options related to downloading and caching
    shiny::shinyOptions(
        age = age,
        destdir = destdir,
        argoServer = server, # rename server to avoid shiny problem
        colLand = colLand,
        debug = debug
    )
    dir <- system.file("shiny", "mapApp/app.R", package = "argoFloats")
    # dir <- "~/git/argoFloats/inst/shiny/mapApp"
    if (!nchar(dir)) {
        stop("The app could not be located.", call. = FALSE)
    }
    shiny::runApp(dir, display.mode = "normal")
}
