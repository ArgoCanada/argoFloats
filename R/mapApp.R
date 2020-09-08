## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

if (!require("shiny"))
    stop("must install.packages(\"shiny\") for this to work")
if (!require("shinycssloaders"))
    stop("must install.packages(\"shinycssloaders\") for this to work")

##cacheAge <- 5                          # a global variable
col <- list(core = 7, bgc = 3, deep = 6)

## Start and end times, covering 21 days to usually get 2 cycles
endTime <- as.POSIXlt(Sys.time())
startTime <- as.POSIXlt(endTime - 21 * 86400)

#' @importFrom grDevices grey
#' @importFrom graphics arrows image lines mtext

uiMapApp  <- shiny::fluidPage(
    shiny::headerPanel(title = "", windowTitle = "argoFloats mapApp"),
    shiny::tags$script(
        '$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'
    ),
    style = "text-indent:1em; background:#e6f3ff ; .btn.disabled { background-color: red; }",
    shiny::fluidRow(
        shiny::column(4, shiny::p("mapApp"), style = "color:blue;"),
        shiny::column(1, shiny::actionButton("help", "Help")),
        shiny::column(1, shiny::actionButton("code", "Code"))
    ),
    shiny::fluidRow(
        shiny::column(2, shiny::textInput(
            "start",
            "Start",
            value = sprintf(
                "%4d-%02d-%02d",
                startTime$year + 1900,
                startTime$mon + 1,
                startTime$mday
            )
        )),
        shiny::column(2,
                      shiny::textInput(
                          "end",
                          "End",
                          value = sprintf(
                              "%4d-%02d-%02d",
                              endTime$year + 1900,
                              endTime$mon + 1,
                              endTime$mday
                          )
                      )),
        shiny::column(
            4,
            style = "padding-left:0px;",
            shiny::checkboxGroupInput(
                "view",
                label = "View",
                choices = c(
                    "Core" = "core",
                    "Deep" = "deep",
                    "BGC" = "bgc",
                    "HiRes" = "hires",
                    "Topo." = "topo",
                    "Path" = "path"
                ),
                selected = c("core", "deep", "bgc"),
                inline = TRUE
            )
        ),
        shiny::column(
            4,
            style = "margin-top: 20px;",
            shiny::actionButton("goS", "\U21E9"),
            shiny::actionButton("goN", "\U21E7"),
            shiny::actionButton("goW", "\U21E6"),
            shiny::actionButton("goE", "\U21E8"),
            shiny::actionButton("zoomIn", "+"),
            shiny::actionButton("zoomOut", "-")
        )
    ),
    shiny::fluidRow(
        shiny::column(2, shiny::textInput("id", "Float ID", value = "")),
        shiny::column(
            2,
            style = "padding-left:0px;",
            shiny::selectInput(
                "focus",
                "Focus",
                choices =
                    c("All" = "all", "Single" = "single"),
                selected =
                    "all"
            )
        ),
        shiny::column(8, shiny::verbatimTextOutput("info"))
    ),
    shiny::fluidRow(shinycssloaders::withSpinner(
        plotOutput(
            "plotMap",
            hover = shiny::hoverOpts("hover"),
            dblclick = shiny::dblclickOpts("dblclick"),
            brush = shiny::brushOpts("brush", delay = 2000, resetOnNew = TRUE)
        )
    ))
)

serverMapApp <- function(input, output, session) {
    AGE <- 7
    ## State variable: reactive!
    state <- shiny::reactiveValues(
        xlim = c(-180, 180),
        ylim = c(-90, 90),
        startTime = startTime,
        endTime = endTime,
        focusID = NULL
    )
    ## Depending on whether 'hires' selected, 'coastline' wil be one of the following two version:
    data("coastlineWorld", package = "oce", envir = environment())
    coastlineWorld <- get("coastlineWorld")
    data("coastlineWorldFine",
         package = "ocedata",
         envir = environment())
    coastlineWorldFine <- get("coastlineWorldFine")
    ## Depending on whether 'hires' selected, 'topo' wil be one of the following two version:
    data("topoWorld", package = "oce", envir = environment())
    topoWorld <- get("topoWorld")
    
    ## Get data.  Since this is slow, we will cache locally into a file named argo.rda, and use that if
    ## it is young enough.  The FALSE part of this loop constructs and saves such a file.
    if (file.exists("argo.rda") &&
        (as.integer(Sys.time()) - as.integer(file.info("argo.rda")$mtime)) / 86400 < AGE) {
        load("argo.rda")
    } else {
        i <- argoFloats::getIndex(age = AGE)
        n <- i[["length"]]
        id <- i[["id"]]
        cycle <- i[["cycle"]]
        lon <- i[["longitude"]]
        lat <- i[["latitude"]]
        n <- length(id)
        iBGC <- argoFloats::getIndex("bgc", age = AGE)
        idBGC <- unique(iBGC[["id"]])
        type <- rep("core", n)
        type[id %in% idBGC] <- "bgc"
        type[grepl("849|862|864", i@data$index$profiler_type)] <-
            "deep"
        argo <-
            data.frame(
                time = i[["date"]],
                id = id,
                cycle = cycle,
                longitude = lon,
                latitude = lat,
                type = type
            )
        argo$longitude <-
            ifelse(argo$longitude > 180,
                   argo$longitude - 360,
                   argo$longitude)
        n <- length(argo$longitude)
        ok <- is.finite(argo$time)
        argo <- argo[ok,]
        n <- length(argo$longitude)
        ok <- is.finite(argo$longitude)
        argo <- argo[ok,]
        n <- length(argo$longitude)
        n <- length(argo$latitude)
        ok <- is.finite(argo$latitude)
        argo <- argo[ok,]
        save(argo, file = "argo.rda")
    }
    ## vector indicating whether to keep any given cycle.
    visible <- rep(TRUE, length(argo$lon))
    
    ## Prevent off-world points
    pinlat <- function(lat)
        ifelse(lat < -90, -90, ifelse(90 < lat, 90, lat))
    pinlon <- function(lon)
        ifelse(lon < -180, -180, ifelse(180 < lon, 180, lon))
    ## Show an error instead of a plot
    showError <- function(msg)
    {
        plot(
            0:1,
            0:1,
            xlab = "",
            ylab = "",
            type = "n",
            axes = FALSE
        )
        text(0.5, 0.5, msg, col = 2, font = 2)
    }
    
    output$info <-
        shiny::renderText({
            # show location.  If lat range is under 90deg, also show nearest float within 100km
            x <- input$hover$x
            y <- input$hover$y
            lonstring <-
                ifelse(x < 0, sprintf("%.2fW", abs(x)), sprintf("%.2fE", x))
            latstring <-
                ifelse(y < 0, sprintf("%.2fS", abs(y)), sprintf("%.2fN", y))
            if (diff(range(state$ylim)) < 90 && sum(visible)) {
                fac <-
                    1 / cos(y * pi / 180) ^ 2 # for deltaLon^2 compared with deltaLat^2
                dist2 <-
                    ifelse(visible,
                           fac * (x - argo$longitude) ^ 2 + (y - argo$latitude) ^ 2,
                           1000)
                i <- which.min(dist2)
                dist <-
                    sqrt(dist2[i]) * 111 # 1deg lat approx 111km
                if (length(dist) && dist < 100) {
                    sprintf(
                        "%s %s, %.0f km from %s float with ID %s\n  at cycle %s [%s]",
                        lonstring,
                        latstring,
                        dist,
                        argo$type[i],
                        argo$id[i],
                        argo$cycle[i],
                        format(argo$time[i], "%Y-%m-%d %H:%M")
                    )
                } else {
                    sprintf("%s %s", lonstring, latstring)
                }
            } else {
                sprintf("%s %s", lonstring, latstring)
            }
        })
    
    shiny::observeEvent(input$goE,
                        {
                            dx <- diff(state$xlim) # present longitude span
                            state$xlim <<-
                                pinlat(state$xlim + dx / 4)
                        })
    
    shiny::observeEvent(input$goW,
                        {
                            dx <- diff(state$xlim) # present longitude span
                            state$xlim <<-
                                pinlat(state$xlim - dx / 4)
                        })
    
    shiny::observeEvent(input$goS,
                        {
                            dy <- diff(state$ylim) # present latitude span
                            state$ylim <<-
                                pinlat(state$ylim - dy / 4)
                        })
    
    shiny::observeEvent(input$goN,
                        {
                            dy <- diff(state$ylim) # present latitude span
                            state$ylim <<-
                                pinlat(state$ylim + dy / 4)
                        })
    
    shiny::observeEvent(input$zoomIn,
                        {
                            state$xlim <<-
                                pinlon(mean(state$xlim)) + c(-0.5, 0.5) / 1.3 * diff(state$xlim)
                            state$ylim <<-
                                pinlat(mean(state$ylim)) + c(-0.5, 0.5) / 1.3 * diff(state$ylim)
                        })
    
    shiny::observeEvent(input$zoomOut,
                        {
                            state$xlim <<-
                                pinlon(mean(state$xlim) + c(-0.5, 0.5) * 1.3 * diff(state$xlim))
                            state$ylim <<-
                                pinlat(mean(state$ylim) + c(-0.5, 0.5) * 1.3 * diff(state$ylim))
                        })
    
    shiny::observeEvent(input$code,
                        {
                            msg <- "library(argoFloats)<br>"
                            msg <-
                                paste(msg,
                                      "# Download (or use cached) index from one of two international servers.<br>")
                            msg <-
                                paste(msg, "index <- getIndex()<br>")
                            msg <-
                                paste(msg, "# Subset by time.<br>")
                            msg <-
                                paste(
                                    msg,
                                    "from <- as.POSIXct(\"",
                                    format(state$startTime, "%Y-%m-%d"),
                                    "\", tz=\"UTC\")<br>",
                                    sep = ""
                                )
                            msg <-
                                paste(
                                    msg,
                                    "to <- as.POSIXct(\"",
                                    format(state$endTime, "%Y-%m-%d"),
                                    "\", tz=\"UTC\")<br>",
                                    sep = ""
                                )
                            msg <-
                                paste(msg,
                                      "subset1 <- subset(index, time=list(from=from, to=to))<br>")
                            msg <-
                                paste(msg, "# Subset by space.<br>", sep = "")
                            lonRect <- state$xlim
                            latRect <- state$ylim
                            msg <-
                                paste(
                                    msg,
                                    sprintf(
                                        "rect <- list(longitude=c(%.4f,%.4f), latitude=c(%.4f,%.4f))<br>",
                                        lonRect[1],
                                        lonRect[2],
                                        latRect[1],
                                        latRect[2]
                                    )
                                )
                            msg <-
                                paste(msg, "subset2 <- subset(subset1, rectangle=rect)<br>")
                            msg <-
                                paste(msg,
                                      "# Plot a map (with different formatting than used here).<br>")
                            msg <-
                                paste(msg, "plot(subset2, which=\"map\")<br>")
                            msg <-
                                paste(msg,
                                      "# The following shows how to make a TS plot for these profiles. This<br>")
                            msg <-
                                paste(msg,
                                      "# involves downloading, which will be slow for a large number of<br>")
                            msg <-
                                paste(msg,
                                      "# profiles, so the steps are placed in an unexecuted block.<br>")
                            msg <-
                                paste(msg, "if (FALSE) {<br>")
                            msg <-
                                paste(msg,
                                      "&nbsp;&nbsp; profiles <- getProfiles(subset2)<br>")
                            msg <-
                                paste(msg,
                                      "&nbsp;&nbsp; argos <- readProfiles(profiles)<br>")
                            msg <-
                                paste(
                                    msg,
                                    "&nbsp;&nbsp; plot(applyQC(argos), which=\"TS\", TScontrol=list(colByCycle=1:8))<br>"
                                )
                            msg <- paste(msg, "}<br>")
                            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title =
                                                                    "R code", size = "l"))
                        })
    
    shiny::observeEvent(input$id,
                        {
                            if (0 == nchar(input$id)) {
                                state$focusID <<- NULL
                                shiny::updateTextInput(session, "focus", value =
                                                           "all")
                            } else {
                                k <- which(argo$id == input$id)
                                if (length(k)) {
                                    state$focusID <<- input$id
                                    state$xlim <<-
                                        pinlon(extendrange(argo$lon[k], f = 0.15))
                                    state$ylim <<-
                                        pinlat(extendrange(argo$lat[k], f = 0.15))
                                    if (input$focus == "all")
                                        shiny::showNotification(
                                            paste0(
                                                "Since you entered a float ID (",
                                                input$id,
                                                "), you might want to change Focus to \"Single\""
                                            ),
                                            type = "message",
                                            duration = 10
                                        )
                                } else {
                                    shiny::showNotification(paste0("There is no float with ID ", input$id, "."),
                                                            type = "error")
                                }
                            }
                        })
    
    shiny::observeEvent(input$focus,
                        {
                            if (input$focus == "single") {
                                if (is.null(state$focusID)) {
                                    shiny::showNotification(
                                        "Double-click on a point or type an ID in the 'Flost ID' box, to single out a focus float",
                                        type = "error",
                                        duration = NULL
                                    )
                                } else {
                                    k <- argo$id == state$focusID
                                    ## Extend the range 3X more than the default, because I almost always
                                    ## end up typing "-" a few times to zoom out
                                    state$xlim <<-
                                        pinlon(extendrange(argo$lon[k], f = 0.15))
                                    state$ylim <<-
                                        pinlat(extendrange(argo$lat[k], f = 0.15))
                                    state$startTime <<-
                                        min(argo$time[k])
                                    state$endTime <<-
                                        max(argo$time[k])
                                    shiny::updateTextInput(session,
                                                           "start",
                                                           value = format(state$startTime, "%Y-%m-%d"))
                                    shiny::updateTextInput(session,
                                                           "end",
                                                           value = format(state$endTime, "%Y-%m-%d"))
                                }
                            } else {
                                # "all"
                                shiny::updateTextInput(session, "id", value =
                                                           "")
                                state$focusID <<- NULL
                            }
                        })
    
    shiny::observeEvent(input$dblclick,
                        {
                            x <- input$dblclick$x
                            y <- input$dblclick$y
                            fac <-
                                1 / cos(y * pi / 180) ^ 2 # for deltaLon^2 compared with deltaLat^2
                            if (input$focus == "single" &&
                                !is.null(state$focusID)) {
                                keep <- argo$id == state$focusID
                            } else {
                                ## Restrict search to the present time window
                                keep <-
                                    state$startTime <= argo$time &
                                    argo$time <= state$endTime
                            }
                            i <-
                                which.min(ifelse(
                                    keep,
                                    fac * (x - argo$longitude) ^ 2 + (y - argo$latitude) ^ 2,
                                    1000
                                ))
                            state$focusID <<- argo$id[i]
                            shiny::updateTextInput(session, "id", value =
                                                       state$focusID)
                            msg <-
                                sprintf(
                                    "ID %s, cycle %s<br>%s %.3fE %.3fN",
                                    argo$id[i],
                                    argo$cycle[i],
                                    format(argo$time[i], "%Y-%m-%d"),
                                    argo$longitude[i],
                                    argo$latitude[i]
                                )
                            shiny::showNotification(shiny::HTML(msg), duration =
                                                        NULL)
                        })
    
    shiny::observeEvent(input$start,
                        {
                            if (0 == nchar(input$start)) {
                                state$startTime <<- min(argo$time, na.rm = TRUE)
                            } else {
                                t <- try(as.POSIXct(input$start, tz = "UTC"), silent = TRUE)
                                if (inherits(t, "try-error")) {
                                    shiny::showNotification(
                                        paste0(
                                            "Start time \"",
                                            input$start,
                                            "\" is not in yyyy-mm-dd format, or is otherwise invalid."
                                        ),
                                        type = "error"
                                    )
                                } else {
                                    state$startTime <<- t
                                }
                            }
                        })
    
    shiny::observeEvent(input$end,
                        {
                            if (0 == nchar(input$end)) {
                                state$endTime <<- max(argo$time, na.rm = TRUE)
                            } else {
                                t <- try(as.POSIXct(input$end, tz = "UTC"), silent = TRUE)
                                if (inherits(t, "try-error")) {
                                    shiny::showNotification(
                                        paste0(
                                            "End time \"",
                                            input$end,
                                            "\" is not in yyyy-mm-dd format, or is otherwise invalid."
                                        ),
                                        type = "error"
                                    )
                                } else {
                                    state$endTime <<- t
                                }
                            }
                        })
    
    shiny::observeEvent(input$help,
                        {
                            msg <-
                                "Enter values in the Start and End boxes to set the time range in numeric yyyy-mm-dd format, or empty either box to use the full time range of the data.<br><br>Use 'View' to select profiles to show (core points are in yellow, deep in purple, and bgc in green), whether to show coastline and topography in high or low resolution, whether to show topography, and whether to connect points to indicate float paths. <br><br>Click-drag the mouse to enlarge a region. Double-click on a particular point to get a popup window giving info on that profile. After such double-clicking, you may change to focus to Single, to see the whole history of that float's trajectory.<br><br>A box above the plot shows the mouse position in longitude and latitude.  If the latitude range is under 90 degrees, and the mouse is within 100km of a float location, that box will also show the float ID and the cycle (profile) number.<br><br>The \"R code\" button brings up a window showing R code that isolates to the view shown and demonstrates some further operations.<br><br>Type '?' to bring up a window that lists key-stroke commands, for further actions including zooming and shifting the spatial view, and sliding the time window."
                            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title =
                                                                    "Using this application", size = "l"))
                        })                                  # help
    
    shiny::observeEvent(input$keypressTrigger,
                        {
                            key <- intToUtf8(input$keypress)
                            if (key == "n") {
                                dy <- diff(state$ylim) # present latitude span
                                state$ylim <<-
                                    pinlat(state$ylim + dy / 4)
                            } else if (key == "s") {
                                dy <- diff(state$ylim) # present latitude span
                                state$ylim <<-
                                    pinlat(state$ylim - dy / 4)
                            } else if (key == "e") {
                                dx <- diff(state$xlim) # present latitude span
                                state$xlim <<-
                                    pinlon(state$xlim + dx / 4)
                            } else if (key == "w") {
                                dx <- diff(state$xlim) # present latitude span
                                state$xlim <<-
                                    pinlon(state$xlim - dx / 4)
                            } else if (key == "f") {
                                # forward in time
                                interval <-
                                    as.numeric(state$endTime) - as.numeric(state$startTime)
                                state$startTime <<-
                                    state$startTime + interval
                                state$endTime <<-
                                    state$endTime + interval
                                shiny::updateTextInput(session,
                                                       "start",
                                                       value = format(state$startTime, "%Y-%m-%d"))
                                shiny::updateTextInput(session, "end", value =
                                                           format(state$endTime, "%Y-%m-%d"))
                            } else if (key == "b") {
                                # backward in time
                                interval <-
                                    as.numeric(state$endTime) - as.numeric(state$startTime)
                                state$startTime <<-
                                    state$startTime - interval
                                state$endTime <<-
                                    state$endTime - interval
                                shiny::updateTextInput(session,
                                                       "start",
                                                       value = format(state$startTime, "%Y-%m-%d"))
                                shiny::updateTextInput(session, "end", value =
                                                           format(state$endTime, "%Y-%m-%d"))
                            } else if (key == "i") {
                                # zoom in
                                state$xlim <<-
                                    pinlon(mean(state$xlim)) + c(-0.5, 0.5) / 1.3 * diff(state$xlim)
                                state$ylim <<-
                                    pinlat(mean(state$ylim)) + c(-0.5, 0.5) / 1.3 * diff(state$ylim)
                            } else if (key == "o") {
                                state$xlim <<-
                                    pinlon(mean(state$xlim) + c(-0.5, 0.5) * 1.3 * diff(state$xlim))
                                state$ylim <<-
                                    pinlat(mean(state$ylim) + c(-0.5, 0.5) * 1.3 * diff(state$ylim))
                            } else if (key == "r") {
                                # reset to start
                                state$xlim <<- c(-180, 180)
                                state$ylim <<- c(-90, 90)
                                state$startTime <<- startTime
                                state$endTime <<- endTime
                                state$focusID <<- NULL
                                shiny::updateSelectInput(session, "focus", selected =
                                                             "all")
                                shiny::updateCheckboxGroupInput(session, "show", selected =
                                                                    character(0))
                            } else if (key == "?") {
                                shiny::showModal(
                                    shiny::modalDialog(
                                        title = "Key-stroke commands",
                                        shiny::HTML(
                                            "<ul>
                                                                                <li> '<b>i</b>': zoom <b>i</b>n</li>
                                                                                <li> '<b>o</b>': zoom <b>o</b>ut</li>
                                                                                <li> '<b>n</b>': go <b>n</b>orth</li>
                                                                                <li> '<b>e</b>': go <b>e</b>ast</li>
                                                                                <li> '<b>s</b>': go <b>s</b>outh</li>
                                                                                <li> '<b>w</b>': go <b>w</b>est</li>
                                                                                <li> '<b>f</b>': go <b>f</b>orward in time</li>
                                                                                <li> '<b>b</b>': go <b>b</b>ackward in time </li>
                                                                                <li> '<b>r</b>': <b>r</b>eset to initial state</li>
                                                                                <li> '<b>?</b>': display this message</li>
                                                                                </ul>"
                                        ),
                                        easyClose = TRUE
                                    )
                                )
                            }
                        })                                  # keypressTrigger
    
    output$plotMap <- shiny::renderPlot({
        start <- state$startTime
        end <- state$endTime
        if (start > end) {
            showError(
                paste0(
                    "Start must precede End , but got Start=",
                    format(start, "%Y-%m-%d"),
                    " and End=",
                    format(end, "%Y-%m-%d.")
                )
            )
        } else {
            if (!is.null(input$brush)) {
                ## Require a minimum size, to avoid mixups with minor click-slide
                if ((input$brush$xmax - input$brush$xmin) > 0.5 &&
                    (input$brush$ymax - input$brush$ymin) > 0.5) {
                    state$xlim <<- c(input$brush$xmin, input$brush$xmax)
                    state$ylim <<-
                        c(input$brush$ymin, input$brush$ymax)
                }
            }
            if (0 == sum(c("core", "deep", "bgc") %in% input$view)) {
                showError("Please select at least 1 type")
            } else {
                par(mar = c(2.5, 2.5, 2, 1.5))
                plot(
                    state$xlim,
                    state$ylim,
                    xlab = "",
                    ylab = "",
                    axes = FALSE,
                    type = "n",
                    asp = 1 / cos(pi / 180 * mean(state$ylim))
                )
                ## FIXME: select hi or low res next
                if ("topo" %in% input$view) {
                    image(
                        topoWorld[["longitude"]],
                        topoWorld[["latitude"]],
                        topoWorld[["z"]],
                        add = TRUE,
                        breaks = seq(-8000, 0, 100),
                        col = oce::oceColorsGebco(80)
                    )
                }
                usr <- par("usr")
                usr[1] <- pinlon(usr[1])
                usr[2] <- pinlon(usr[2])
                usr[3] <- pinlat(usr[3])
                usr[4] <- pinlat(usr[4])
                at <- pretty(usr[1:2], 10)
                at <- at[usr[1] < at & at < usr[2]]
                labels <-
                    paste(abs(at), ifelse(at < 0, "W", ifelse(at > 0, "E", "")), sep = "")
                axis(
                    1,
                    pos = pinlat(usr[3]),
                    at = at,
                    labels = labels,
                    lwd = 1
                )
                at <- pretty(usr[3:4], 10)
                at <- at[usr[3] < at & at < usr[4]]
                labels <-
                    paste(abs(at), ifelse(at < 0, "S", ifelse(at > 0, "N", "")), sep = "")
                axis(
                    2,
                    pos = pinlon(usr[1]),
                    at = at,
                    labels = labels,
                    lwd = 1
                )
                coastline <-
                    if ("hires" %in% input$view)
                        coastlineWorldFine
                else
                    coastlineWorld
                polygon(coastline[["longitude"]], coastline[["latitude"]], col =
                            "tan")
                rect(usr[1], usr[3], usr[2], usr[4], lwd = 1)
                ## For focusID mode, we do not trim by time or space
                if (input$focus == "single" &&
                    !is.null(state$focusID)) {
                    keep <- argo$id == state$focusID
                }  else {
                    keep <- rep(TRUE, length(argo$id))
                }
                keep <-
                    keep &
                    (state$startTime <= argo$time &
                         argo$time <= state$endTime)
                keep <-
                    keep &
                    (state$xlim[1] <= argo$longitude &
                         argo$longitude <= state$xlim[2])
                keep <-
                    keep &
                    (state$ylim[1] <= argo$latitude &
                         argo$latitude <= state$ylim[2])
                cex <- 0.75
                ## Draw points, optionally connecting paths (and indicating start points)
                ## {{{
                visible <<- rep(FALSE, length(argo$lon))
                for (view in c("core", "deep", "bgc")) {
                    if (view %in% input$view) {
                        k <- keep & argo$type == view
                        visible <<- visible | k
                        lonlat <- argo[k, ]
                        points(
                            lonlat$lon,
                            lonlat$lat,
                            pch = 21,
                            cex = cex,
                            col = "black",
                            bg = col[[view]],
                            lwd = 0.5
                        )
                        if ("path" %in% input$view) {
                            ## Turn off warnings for zero-length arrows
                            owarn <- options("warn")$warn
                            options(warn = -1)
                            for (ID in unique(lonlat$id)) {
                                LONLAT <- subset(lonlat, ID == id)
                                ## Sort by time instead of relying on the order in the repository
                                o <- order(LONLAT$time)
                                no <- length(o)
                                if (no > 1) {
                                    LONLAT <- LONLAT[o,]
                                    lines(
                                        LONLAT$lon,
                                        LONLAT$lat,
                                        lwd = 1,
                                        col = grey(0.3)
                                    )
                                    arrows(
                                        LONLAT$lon[no - 1],
                                        LONLAT$lat[no - 1],
                                        LONLAT$lon[no],
                                        LONLAT$lat[no],
                                        length = 0.15,
                                        lwd = 1.4
                                    )
                                }
                            }
                            par(warn = owarn)
                        }
                    }
                }
                ## }}}
                ## Draw the inspection rectangle as a thick gray line, but only if zoomed
                if (-180 < state$xlim[1] ||
                    state$xlim[2] < 180 ||
                    -90 < state$ylim[1] || state$ylim[2] < 90)
                    rect(
                        state$xlim[1],
                        state$ylim[1],
                        state$xlim[2],
                        state$ylim[2],
                        border = "darkgray",
                        lwd = 4
                    )
                ## Write a margin comment
                if (input$focus == "single" &&
                    !is.null(state$focusID)) {
                    mtext(
                        paste("Float ID", state$focusID),
                        cex = 0.8 * par("cex"),
                        line = 0.25
                    )
                } else {
                    mtext(
                        side = 3,
                        sprintf(
                            "%s to %s: %d Argo profiles",
                            format(start, "%Y-%m-%d"),
                            format(end, "%Y-%m-%d"),
                            sum(visible)
                        ),
                        line = 0.25,
                        cex = 0.8 * par("cex")
                    )
                }
            }
        }
    }, height = 500, pointsize = 18)       # plotMap
}                                 # server

#' Interactive app for viewing argo float positions
#'
#' The GUI permits specifying a spatial-temporal region of interest, a set
#' of float types to show, etc.  The interface ought to be reasonably
#' straightforward, especially for those who take a moment to click on the
#' Help button and to read the popup window that it creates.
#'
#' This app will use [getIndex()] to download index files from the
#' Argo server the first time it runs.  Then it will combine information from
#' the core-argo and bgc-argo index tables, cross-indexing so it can determine
#" the argo type for each profile (or cycle).  The results of this combination
#' are stored in a local file named `argo.rda`, which is then used for the functioning
#' of the app.  In the interests of speed, a check is made on startup for the
#' existence of this file, and it will be reused if it is less than 7 days old.
#'
## @param age a numerical value that gives the maximum permitted age (in
## days) of a local cache file named `argo.rda`; see \dQuote{Details}.
#'
#' @examples
#'\dontrun{
#' library(argoFloats)
#' mapApp()}
#'
#' @author Dan Kelley
#' @export
#mapApp <- function(age = 7)
mapApp <- function()
{
    ##cacheAge <<- age
    print(shiny::shinyApp(ui = uiMapApp, server = serverMapApp))
}

#shiny::shinyApp(ui, server)
