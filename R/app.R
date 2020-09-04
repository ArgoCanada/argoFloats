## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(oce)
library(argoFloats)
library(shiny)

## Convenience functions that print to the R console, mainly useful during development.
msg <- function(...)
    cat(file=stderr(), ...)
dmsg <- function(...)
    if (debug) cat(file=stderr(), ...)

## Global variables

debug <- TRUE                          # if TRUE, dmsg() will print to console for debugging

author <- "Kelley, Dan"
affiliation <- "Dalhousie University Physical Oceanography"
title <- "Argo Map"
version <- "0.1"
time <- "2020 Sep 4"
copyright <- "(c) 2020 Dan Kelley"
url <- "https://services.dal.ace-net.ca/shiny/dkelley/2020/argo_map/"

data(coastlineWorld)
data(topoWorld)
longitudeCoastline <- coastlineWorld[["longitude"]]
latitudeCoastline <- coastlineWorld[["latitude"]]


endTime <- as.POSIXlt(Sys.time())
startTime <- as.POSIXlt(endTime - 21*86400) # default to 21d, which will usually include 2 cycles


## Get data
if (file.exists("argo.rda")) {
    msg("Using data cached in \"argo.rda\"\n")
    load("argo.rda")
} else {
    i <- getIndex()
    n <- i[["length"]]
    id <- i[["id"]]
    cycle <- i[["cycle"]]
    lon <- i[["longitude"]]
    lat <- i[["latitude"]]
    n <- length(id)
    iBGC <- getIndex("bgc")
    idBGC <- unique(iBGC[["id"]])
    type <- rep("core", n)
    type[id %in% idBGC] <- "bgc"
    type[grepl("849|862|864", i@data$index$profiler_type)] <- "deep"
    argo <- data.frame(time=i[["date"]], id=id, cycle=cycle, longitude=lon, latitude=lat, type=type)
    argo$longitude <- ifelse(argo$longitude > 180, argo$longitude-360, argo$longitude)
    n <- length(argo$longitude)
    msg(sprintf("Have %d data, originally\n", n))
    ok <- is.finite(argo$time)
    argo <- argo[ok,]
    msg(sprintf("Have %d data, after trimming the %.3f%% of data that have non-finite times\n", n, 100*sum(!ok)/length(ok)))
    n <- length(argo$longitude)
    ok <- is.finite(argo$longitude)
    argo <- argo[ok,]
    n <- length(argo$longitude)
    msg(sprintf("Have %d data, after trimming the %.3f%% of data that have non-finite longitudes\n", n, 100*sum(!ok)/length(ok)))
    n <- length(argo$latitude)
    ok <- is.finite(argo$latitude)
    argo <- argo[ok,]
    msg(sprintf("Have %d data, after trimming the %.3f%% of data that have non-finite latitudes\n", n, 100*sum(!ok)/length(ok)))
    msg("saving data in \"argo.rda\"\n")
    save(argo, file="argo.rda")
}

visible <- rep(TRUE, length(argo$lon))

ui <- fluidPage(headerPanel(title="", windowTitle=title),
                tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
                style="text-indent:1em; background:#e6f3ff",
                fluidRow(column(3, p(paste0(title, " [", version, "]")),
                                style="font-weight:bold; color:blue;"),
                         column(3, p(copyright), style="color:blue;"),
                         column(1, actionButton("help", "Help")),
                         column(2, actionButton("citation", "Citation")),
                         column(2, actionButton("code", "Code"))),
                fluidRow(column(3, textInput("start", "Start (yyyy-mm-dd)",
                                             value=sprintf("%4d-%02d-%02d",
                                                           startTime$year+1900, startTime$mon+1, startTime$mday))),
                         column(3, textInput("end", "End (yyyy-mm-dd)",
                                             value=sprintf("%4d-%02d-%02d",
                                                           endTime$year+1900, endTime$mon+1, endTime$mday))),
                         column(4, checkboxGroupInput("type", label="Type",
                                                      choices=c("Core"="core", "Deep"="deep", "BGC"="bgc"),
                                                      selected=c("core", "deep", "bgc"),
                                                      inline=TRUE))),
                fluidRow(column(2, textInput("id", "Float ID", value="")),
                         column(2, selectInput("focus", "Focus",
                                               choices=c("All"="all", "Single"="single"),
                                               selected="all")),
                         column(2, checkboxGroupInput("show", label="Show",
                                                      choices=c("Topography"="topography", "Path"="path"),
                                                      selected=NULL)),
                         column(6, verbatimTextOutput("info"))),
                fluidRow(plotOutput("plotMap",
                                    ##click=clickOpts("click"),
                                    hover=hoverOpts("hover"),
                                    dblclick=dblclickOpts("dblclick"),
                                    brush=brushOpts("brush", delay=2000, resetOnNew=TRUE))))


server <- function(input, output, session)
{
    ## Non-shiny functions
    pinlat <- function(lat)
        ifelse(lat < -90, -90, ifelse(90 < lat, 90, lat))
    pinlon <- function(lon)
        ifelse(lon < -180, -180, ifelse(180 < lon, 180, lon))
    showError <- function(msg)
    {
        plot(0:1, 0:1, xlab="", ylab="", type="n", axes=FALSE)
        text(0.5, 0.5, msg, col=2, font=2)
    }

    ## State variable: reactive!
    state <- reactiveValues(xlim=c(-180, 180), ylim=c(-90, 90), startTime=startTime, endTime=endTime, focusID=NULL)

    ## Colours for dot interiors
    colours <- list(core="yellow", bgc="green", deep="purple")

    ## Observers
    output$info <- renderText({ # show location.  If lat range is under 90deg, also show nearest float within 100km
        x <- input$hover$x
        y <- input$hover$y
        lonstring <- ifelse(x < 0, sprintf("%.2fW", abs(x)), sprintf("%.2fE", x))
        latstring <- ifelse(y < 0, sprintf("%.2fS", abs(y)), sprintf("%.2fN", y))
        if (diff(range(state$ylim)) < 90 && sum(visible)) {
            fac <- 1 / cos(y * pi / 180)^2 # for deltaLon^2 compared with deltaLat^2
            dist2 <- ifelse(visible, fac*(x - argo$longitude)^2 + (y - argo$latitude)^2, 1000)
            i <- which.min(dist2)
            dist <- sqrt(dist2[i])*111 # 1deg lat approx 111km
            ID <- argo$id[i]
            cycle <- argo$cycle[i]
            time <- argo$time[i]
            ##paste(sprintf("%.3fN %.3fE id=%s cycle=%s", input$hover$x, input$hover$y, ID, cycle))
            if (length(dist) && dist < 100) {
                sprintf("%s %s, %.0f km from ID %s, cycle %s\n%s", lonstring, latstring, dist, ID, cycle, format(time, "%Y-%m-%d"))
            } else {
                sprintf("%s %s", lonstring, latstring)
            }
        } else {
            sprintf("%s %s", lonstring, latstring)
        }
    })

    observeEvent(input$code,
                 {
                     showNotification("FIXME: code here", type="message", duration=10)
                 }

    )

    observeEvent(input$id,
                 {
                     if (0 == nchar(input$id)) {
                         state$focusID <<- NULL
                         updateTextInput(session, "focus", value="all")
                     } else {
                         if (input$focus == "all")
                             showNotification(paste0("Since you entered a float ID (", input$id, "), you might want to change Focus to \"Single\""), type="message", duration=10)
                         state$focusID <<- input$id
                     }
                 }
    )

    observeEvent(input$focus,
                 {
                     ## dmsg("in focus observer; input$focus=", input$focus, "\n")
                     if (input$focus == "single") {
                         if (is.null(state$focusID)) {
                             showNotification("Double-click on a point to register a focus float", type="error", duration=NULL)
                         } else {
                             k <- argo$id == state$focusID
                             ## Extend the range 3X more than the default, because I almost always
                             ## end up typing "-" a few times to zoom out
                             state$xlim <<- pinlon(extendrange(argo$lon[k], f=0.15))
                             state$ylim <<- pinlat(extendrange(argo$lat[k], f=0.15))
                             state$startTime <<- min(argo$time[k])
                             state$endTime <<- max(argo$time[k])
                             updateTextInput(session, "start", value=format(state$startTime, "%Y-%m-%d"))
                             updateTextInput(session, "end", value=format(state$endTime, "%Y-%m-%d"))
                         }
                     } else { # "all"
                         state$xlim <<- c(-180, 180)
                         state$ylim <<- c(-90, 90)
                         state$startTime <<- startTime
                         state$endTime <<- endTime
                         updateTextInput(session, "id", value="")
                         updateTextInput(session, "start", value=format(state$startTime, "%Y-%m-%d"))
                         updateTextInput(session, "end", value=format(state$endTime, "%Y-%m-%d"))
                         state$focusID <<- NULL
                     }
                 }
    )

    observeEvent(input$dblclick,
                 {
                     x <- input$dblclick$x
                     y <- input$dblclick$y
                     fac <- 1 / cos(y * pi / 180)^2 # for deltaLon^2 compared with deltaLat^2
                     if (input$focus == "single" && !is.null(state$focusID)) {
                         keep <- argo$id == state$focusID
                     } else {
                         ## Restrict search to the present time window
                         keep <- state$startTime <= argo$time & argo$time <= state$endTime
                     }
                     i <- which.min(ifelse(keep, fac*(x - argo$longitude)^2 + (y - argo$latitude)^2, 1000))
                     state$focusID <<- argo$id[i]
                     updateTextInput(session, "id", value=state$focusID)
                     msg <- sprintf("ID %s, cycle %s<br>%s %.3fE %.3fN",
                                    argo$id[i], argo$cycle[i],
                                    format(argo$time[i], "%Y-%m-%d"), argo$longitude[i], argo$latitude[i])
                     showNotification(HTML(msg), duration=NULL)
                 }
    )

    observeEvent(input$start,
                 {
                     if (0 == nchar(input$start)) {
                         state$startTime <<- min(argo$time, na.rm=TRUE)
                     } else {
                         t <- try(as.POSIXct(input$start, tz="UTC"), silent=TRUE)
                         if (inherits(t, "try-error")) {
                             showNotification(paste0("Start time \"", input$start, "\" is not in yyyy-mm-dd format, or is otherwise invalid."), type="error")
                         } else {
                             state$startTime <<- t
                         }
                     }
                 }
    )

    observeEvent(input$end,
                 {
                     if (0 == nchar(input$end)) {
                         state$endTime <<- max(argo$time, na.rm=TRUE)
                     } else {
                         t <- try(as.POSIXct(input$end, tz="UTC"), silent=TRUE)
                         if (inherits(t, "try-error")) {
                             showNotification(paste0("End time \"", input$end, "\" is not in yyyy-mm-dd format, or is otherwise invalid."), type="error")
                         } else {
                             state$endTime <<- t
                         }
                     }
                 }
    )

    observeEvent(input$help,
                 {
                     showNotification("Enter values in the Start and End boxes to set the time range in numeric yyyy-mm-dd format, or blank either box out to use the full time range. Click-drag the mouse to enlarge a region.  Double-click on a particular point to get a popup window giving info on that profile. After such double-clicking, you may change to focus to Single, to see the whole history of that float's trajectory. Other buttons permit selecting float type, displaying topography, and joining profiles by lines (with an arrow pointing to the most recent location).  The \"R code\" button brings up a window showing R code that isolates to the view shown. Type '?' to bring up a window that lists key-stroke commands, for further actions including zooming the view, sliding the time window, etc.", duration=NULL)
                 }
    )                                  # help

    observeEvent(input$citation,
                 {
                     showNotification(paste0(author, ". ", title, " [version ", version, "], ", affiliation, ", ", time, ". ", url, "."), duration=NULL)
                 }
    )                                  # citation

    observeEvent(input$keypressTrigger, {
                 key <- intToUtf8(input$keypress)
                 ##dmsg("key='",key, "'\n", sep="")
                 if (key == "n") {
                     dy <- diff(state$ylim) # present latitude span
                     state$ylim <<- pinlat(state$ylim + dy / 4)
                 } else if (key == "s") {
                     dy <- diff(state$ylim) # present latitude span
                     state$ylim <<- pinlat(state$ylim - dy / 4)
                 } else if (key == "e") {
                     dx <- diff(state$xlim) # present latitude span
                     state$xlim <<- pinlon(state$xlim + dx / 4)
                 } else if (key == "w") {
                     dx <- diff(state$xlim) # present latitude span
                     state$xlim <<- pinlon(state$xlim - dx / 4)
                 } else if (key == "f") { # forward in time
                     interval <- as.numeric(state$endTime) - as.numeric(state$startTime)
                     state$startTime <<- state$startTime + interval
                     state$endTime <<- state$endTime + interval
                     updateTextInput(session, "start", value=format(state$startTime, "%Y-%m-%d"))
                     updateTextInput(session, "end", value=format(state$endTime, "%Y-%m-%d"))
                 } else if (key == "b") { # backward in time
                     interval <- as.numeric(state$endTime) - as.numeric(state$startTime)
                     state$startTime <<- state$startTime - interval
                     state$endTime <<- state$endTime - interval
                     updateTextInput(session, "start", value=format(state$startTime, "%Y-%m-%d"))
                     updateTextInput(session, "end", value=format(state$endTime, "%Y-%m-%d"))
                 } else if (key == "i") { # zoom in
                     state$xlim <<- pinlon(mean(state$xlim)) + c(-0.5,0.5)/1.3 * diff(state$xlim)
                     state$ylim <<- pinlat(mean(state$ylim)) + c(-0.5,0.5)/1.3 * diff(state$ylim)
                 } else if (key == "o") {
                     state$xlim <<- pinlon(mean(state$xlim) + c(-0.5,0.5)*1.3 * diff(state$xlim))
                     state$ylim <<- pinlat(mean(state$ylim) + c(-0.5,0.5)*1.3 * diff(state$ylim))
                 } else if (key == "r") { # reset to start
                     state$xlim <<- c(-180, 180)
                     state$ylim <<- c(-90, 90)
                     state$startTime <<- startTime
                     state$endTime <<- endTime
                     state$focusID <<- NULL
                     updateSelectInput(session, "focus", selected="All")
                     updateCheckboxGroupInput(session, "show", selected=character(0))
                 } else if (key == "?") {
                     showModal(modalDialog(title="Key-stroke commands",
                                           HTML("<ul>
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
                                                </ul>"), easyClose=TRUE))
                 }
    }
    )                                  # keypressTrigger

    output$plotMap <- renderPlot({
        start <- state$startTime
        end <- state$endTime
        if (start > end) {
            showError(paste0("Start must precede End , but got Start=",
                             format(start,"%Y-%m-%d"), " and End=", format(end, "%Y-%m-%d.")))
        } else {
            if (!is.null(input$brush)) {
                ## Require a minimum size, to avoid mixups with minor click-slide
                if ((input$brush$xmax - input$brush$xmin) > 0.5 &&
                    (input$brush$ymax - input$brush$ymin) > 0.5) {
                    state$xlim <<- c(input$brush$xmin, input$brush$xmax)
                    state$ylim <<- c(input$brush$ymin, input$brush$ymax)
                } else {
                    dmsg("skipping accidental brush (based on size under 50km)\n")
                }
            }
            if (0 == length(input$type)) {
                showError("Please select at least 1 type")
            } else {
                par(mar=c(2,2,2,2))
                plot(state$xlim, state$ylim, xlab="", ylab="", axes=FALSE, type="n", asp=1/cos(pi/180*mean(state$ylim)))
                if ("topography" %in% input$show)
                    image(topoWorld[["longitude"]], topoWorld[["latitude"]], topoWorld[["z"]], add=TRUE,
                          breaks=seq(-8000, 0, 100), col=oceColorsGebco(80))
                usr <- par("usr")
                usr[1] <- pinlon(usr[1])
                usr[2] <- pinlon(usr[2])
                usr[3] <- pinlat(usr[3])
                usr[4] <- pinlat(usr[4])
                at <- pretty(usr[1:2], 10)
                at <- at[usr[1] < at & at < usr[2]]
                axis(1, pos=pinlat(usr[3]), at=at, lwd=1)
                at <- pretty(usr[3:4], 10)
                at <- at[usr[3] < at & at < usr[4]]
                axis(2, pos=pinlon(usr[1]), at=at, lwd=1)
                polygon(longitudeCoastline, latitudeCoastline, col="tan")
                rect(usr[1], usr[3], usr[2], usr[4], lwd=1.4)
                ## For focusID mode, we do not trim by time or space
                if (input$focus == "single" && !is.null(state$focusID)) {
                    keep <- argo$id == state$focusID
                }  else {
                    keep <- rep(TRUE, length(argo$id))
                }
                keep <- keep & (state$startTime <= argo$time & argo$time <= state$endTime)
                keep <- keep & (state$xlim[1] <= argo$longitude & argo$longitude <= state$xlim[2])
                keep <- keep & (state$ylim[1] <= argo$latitude & argo$latitude <= state$ylim[2])
                cex <- 0.75
                ## Draw points, optionally connecting paths (and indicating start points)
                ## {{{
                visible <<- rep(FALSE, length(argo$lon))
                for (type in c("core", "deep", "bgc")) {
                    if (type %in% input$type) {
                        k <- keep & argo$type == type
                        visible <<- visible | k
                        lonlat <- argo[k, ]
                        points(lonlat$lon, lonlat$lat, pch=21, cex=cex, col="black", bg=colours[[type]])
                        if ("path" %in% input$show) {
                            ## Turn off warnings for zero-length arrows
                            owarn <- options("warn")$warn
                            options(warn=-1)
                            for (ID in unique(lonlat$id)) {
                                LONLAT <- subset(lonlat, ID==id)
                                ## Sort by time instead of relying on the order in the repository
                                o <- order(LONLAT$time)
                                no <- length(o)
                                if (no > 1) {
                                    LONLAT <- LONLAT[o,]
                                    lines(LONLAT$lon, LONLAT$lat, lwd=1, col=grey(0.3))
                                    arrows(LONLAT$lon[no-1], LONLAT$lat[no-1],
                                           LONLAT$lon[no], LONLAT$lat[no], length=0.15, lwd=1.4)
                                }
                            }
                            par(warn=owarn)
                        }
                    }
                }
                ## }}}
                ## Draw the inspection rectangle as a thick gray line, but only if zoomed
                if (-180 < state$xlim[1] || state$xlim[2] < 180 || -90 < state$ylim[1] || state$ylim[2] < 90)
                    rect(state$xlim[1], state$ylim[1], state$xlim[2], state$ylim[2], border="darkgray", lwd=4)
                ## Write a margin comment
                if (input$focus == "single" && !is.null(state$focusID)) {
                    mtext(paste("Float ID", state$focusID), cex=0.8*par("cex"))
                } else {
                    mtext(side=3, sprintf("%s to %s: %d Argo profiles",
                                          format(start, "%Y-%m-%d"),
                                          format(end, "%Y-%m-%d"),
                                          sum(visible)), line=0.75, cex=0.8*par("cex"))
                }
            }
        }
    }, height=500, pointsize=18)       # plotMap

}                                      # server

shinyApp(ui=ui, server=server)

