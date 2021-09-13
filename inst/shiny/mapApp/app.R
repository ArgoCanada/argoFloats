# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

appName <- "mapApp"
#appVersion <- "0.1"

colDefaults <- list(core="#F5C710", bgc="#05f076", deep="#CD0BBC")

## Default start and end times
endTime <- as.POSIXlt(Sys.time(), tz="UTC")
startTime <- as.POSIXlt(endTime - 10 * 86400)

pi180 <- pi / 180                      # degree/radian conversion factor

keyPressHelp <- "<ul> <li> '<b>i</b>': zoom <b>i</b>n</li>
<li> '<b>o</b>': zoom <b>o</b>ut</li>
<li> '<b>n</b>': go <b>n</b>orth</li>
<li> '<b>e</b>': go <b>e</b>ast</li>
<li> '<b>s</b>': go <b>s</b>outh</li>
<li> '<b>w</b>': go <b>w</b>est</li>
<li> '<b>f</b>': go <b>f</b>orward in time</li>
<li> '<b>b</b>': go <b>b</b>ackward in time</li>
<li> '<b>r</b>': <b>r</b>eset to initial state</li>
<li> '<b>h</b>': hold active hover message (press <b>h</b> again to undo)</li>
<li> '<b>0</b>': Unzoom an area and keep same time scale</li>
<li> '<b>?</b>': display this message</li> </ul>"

overallHelp <- "<p>This app responds to keystroke actions and GUI actions.</p><p>The permitted <u>keystroke actions</u> will be shown in a pop-up window if the <b>?</b> key is pressed. There are keys for zooming in and out, for moving the focus region through space and time, and for controlling updates to an information box that displays mouse location and aspects of a nearby float.</p><p>The <u>GUI actions</u> are reasonably self-explanatory. On the <i>Main tab</i>, users may enter values in the \"Start\" and \"End\" boxes to set the time range of the display, or empty either box to use the data range. The checkboxes of the \"View\" grouping may be used to choose whether to show 'Core', 'Deep' or 'BGC' data, whether to draw a high-resolution coastline, whether to draw connecting line segments to indicate the path of individual floats, and whether to indicate water depth using contour lines. If a path is displayed, there are options to highlight its start and end points, or to hide all points. The focus region may be selected by pressing the mouse at one location, sliding it to a new location, and then releasing it. Double-clicking on a particular float location creates a pop-up window that provides information on that profile. There is a way to focus on an individual float, to the exclusion of others.  Experimenting with the interface will reveal other capabilities; for example, it is worth exploring the <i>Settings tab</i>, which provides control over several aesthetic properties.<p>A text box above the plot shows the mouse position in longitude and latitude as well as information about the nearest profile, if it is within 100km of the mouse location (typing <b>h</b> toggles a setting that causes this information to track the mouse).</p><p>The \"R code\" button brings up a window showing R code that will approximate the view shown in the app, and that hints at some other operations that might be useful in analysis.</p><p>For more details, type <tt>?argoFloats::mapApp</tt> in an R console.</p>"


uiMapApp <- shiny::fluidPage(

    # Header Panel
    shiny::headerPanel(title="", windowTitle="argoFloats mapApp"),
    shiny::tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    style="text-indent:1em; background:#e6f3ff ; .btn.disabled { background-color: red; }",
    shiny::fluidRow(shiny::uiOutput(outputId="UIwidget")),
    shiny::fluidRow(shiny::column(7, shiny::uiOutput(outputId="UIview")),
        shiny::column(2, shiny::uiOutput(outputId="UIID")),
        shiny::column(3, shiny::uiOutput(outputId="UIfocus"))),
    shiny::fluidRow(shiny::uiOutput(outputId="UIinfo")),

    # Main Panel
    shiny::mainPanel(shiny::tabsetPanel(type="tab",
            shiny::tabPanel("Main", value=1),
            shiny::tabPanel("Settings", value=3,
                shiny::tabsetPanel(shiny::tabPanel("Core", value=4, selected=TRUE),
                    shiny::tabPanel("BGC", value=5),
                    shiny::tabPanel("Deep", value=6),
                    id="settab")),
            id="tabselected")),
    shiny::uiOutput(outputId="UItrajectory"),

    shiny::conditionalPanel(condition="input.settab==4 && input.tabselected==3",
        shiny::mainPanel(
            shiny::fluidRow(
                shiny::div(style="color:black; font-weight:bold; margin-bottom: 10px;",
                    hr("Symbol Properties"))),
            shiny::fluidRow(
                shiny::column(3,
                    shiny::div(style="margin-bottom: 10px;",
                        shiny::actionButton("CsymbolGallery", "Symbol Gallery")))),
            shiny::fluidRow(
                shiny::column(3,
                    colourpicker::colourInput("Ccolour", "Colour", colDefaults$core)),
                shiny::column(2, shiny::numericInput("Csymbol", "Type", value=21, min=0, max=25)),
                shiny::column(3, shiny::sliderInput("Csize", "Size", min=0, max=1, value=0.9, step=0.05)),
                shiny::column(3, shiny::conditionalPanel("input.Csymbol== 21",
                        colourpicker::colourInput("Cborder", "Border Colour", "black")))),
            shiny::fluidRow(
                shiny::div(style="color:black; font-weight:bold; margin-bottom: 10px;",
                    hr("Path Properties"))),
            shiny::fluidRow(
                shiny::column(3,
                    colourpicker::colourInput("CPcolour", "Colour", colDefaults$core)),
                shiny::column(3, shiny::sliderInput("CPwidth", "Width", min=0.5, max=2.5, value=1, step=0.1))))),

    shiny::conditionalPanel(condition="input.settab==5 && input.tabselected==3",
        shiny::mainPanel(
            shiny::fluidRow(
                shiny::div(style="color:black; font-weight:bold; margin-bottom: 10px;",
                    hr("Symbol Properties"))),
            shiny::fluidRow(
                shiny::column(3,
                    shiny::div(style="margin-bottom: 10px;",
                        shiny::actionButton("BsymbolGallery", "Symbol Gallery")))),
            shiny::fluidRow(
                shiny::column(3,
                    colourpicker::colourInput("Bcolour", "Colour", colDefaults$bgc)),
                shiny::column(2, shiny::numericInput("Bsymbol", "Type", value=21, min=0, max=25)),
                shiny::column(3, shiny::sliderInput("Bsize", "Size", min=0, max=1, value=0.9, step=0.05)),
                shiny::column(3, shiny::conditionalPanel("input.Csymbol== 21",
                        colourpicker::colourInput("Bborder", "Border Colour", "black")))),
            shiny::fluidRow(
                shiny::div(style="color:black; font-weight:bold; margin-bottom: 10px;",
                    hr("Path Properties"))),
            shiny::fluidRow(
                shiny::column(3,
                    colourpicker::colourInput("BPcolour", "Colour", colDefaults$bgc)),
                shiny::column(3, shiny::sliderInput("BPwidth", "Width", min=0.5, max=2.5, value=1, step=0.1))))),

    shiny::conditionalPanel(condition="input.settab==6 && input.tabselected==3",
        shiny::mainPanel(
            shiny::fluidRow(
                shiny::div(style="color:black; font-weight:bold; margin-bottom: 10px;",
                    hr("Symbol Properties"))),
            shiny::fluidRow(
                shiny::column(3,
                    shiny::div(style="margin-bottom: 10px;",
                        shiny::actionButton("DsymbolGallery", "Symbol Gallery")))),
            shiny::fluidRow(
                shiny::column(3,
                    colourpicker::colourInput("Dcolour", "Colour", colDefaults$deep)),
                shiny::column(2, shiny::numericInput("Dsymbol", "Type", value=21, min=0, max=25)),
                shiny::column(3, shiny::sliderInput("Dsize", "Size", min=0, max=1, value=0.9, step=0.05)),
                shiny::column(3, shiny::conditionalPanel("input.Csymbol== 21",
                        colourpicker::colourInput("Dborder", "Border Colour", "black")))),
            shiny::fluidRow(
                shiny::div(style="color:black; font-weight:bold; margin-bottom: 10px;",
                    hr("Path Properties"))),
            shiny::fluidRow(
                shiny::column(3,
                    colourpicker::colourInput("DPcolour", "Colour", colDefaults$deep)),
                shiny::column(3, shiny::sliderInput("DPwidth", "Width", min=0.5, max=2.5, value=1, step=0.1))))),

    shiny::conditionalPanel("input.tabselected!=3",
        shiny::fluidRow(shiny::plotOutput("plotMap",
                hover=shiny::hoverOpts("hover"),
                dblclick=shiny::dblclickOpts("dblclick"),
                brush=shiny::brushOpts("brush", delay=2000, resetOnNew=TRUE)))))

serverMapApp <- function(input, output, session)
{
    lastHoverMessage <- "" # used with 'p' keystroke
    age <- shiny::getShinyOption("age")
    destdir <- shiny::getShinyOption("destdir")
    argoServer <- shiny::getShinyOption("argoServer")
    colLand <- shiny::getShinyOption("colLand")
    debug <- shiny::getShinyOption("debug")
    if (!requireNamespace("shiny", quietly=TRUE))
        stop("must install.packages('shiny') for mapApp() to work")
    if (!requireNamespace("colourpicker", quietly=TRUE))
        stop("must install.packages('colourpicker') for mapApp() to work")
    ## State variable: reactive!
    state <- shiny::reactiveValues(xlim=c(-180, 180),
        ylim=c(-90, 90),
        startTime=startTime,
        endTime=endTime,
        focus="all",
        action=NULL,
        focusID=NULL,
        view=c("core", "deep", "bgc"),
        hoverIsPasted=FALSE)
    ## Depending on whether 'hires' selected, 'coastline' will be one of the following two version:
    data("coastlineWorld", package="oce", envir=environment())
    coastlineWorld <- get("coastlineWorld")
    data("coastlineWorldFine", package="ocedata", envir=environment())
    coastlineWorldFine <- get("coastlineWorldFine")
    ## Depending on whether 'hires' selected, 'topo' wil be one of the following two version:
    data("topoWorld", package="oce", envir=environment())
    topoWorld <- get("topoWorld")
    if (file.exists("topoWorldFine.rda")) {
        load("topoWorldFine.rda")
    } else {
        topoWorldFine <- topoWorld
    }
    if (argoFloatsIsCached("argo")) {
        notificationId <- shiny::showNotification("Using argo data that were cached temporarily in R-session memory\n")
        argo <- argoFloats::argoFloatsGetFromCache("argo", debug=debug)
    } else {
        ## Get core and BGC data.
        notificationId <- shiny::showNotification("Step 1/3: Getting \"core\" Argo index, either by downloading new data or using data in \"destdir\".  This may take a minute or two.", type="message", duration=NULL)
        i <- argoFloats::getIndex(age=age, destdir=destdir, server=argoServer, debug=debug)
        argoFloatsDebug(debug, "getIndex() returned", if (is.null(i)) "NULL" else "not NULL", "\n")
        shiny::removeNotification(notificationId)
        notificationId <- shiny::showNotification("Step 2/3: Getting \"BGC\" Argo index, either by downloading new data or using cached data.  This may take a minute or two.", type="message", duration=NULL)
        iBGC <- argoFloats::getIndex("bgc", age=age, destdir=destdir, server=argoServer, debug=debug)
        shiny::removeNotification(notificationId)
        # Combine core and BGC data.  This relies on the fact that every BGC ID is
        # also a core ID.  Here is sample code that proves it:
        #
        # library(argoFloats)
        # i <- getIndex()
        # b <- getIndex("bgc")
        # iid <- i[["ID"]]
        # bid <- b[["ID"]]
        # stopifnot(sum(bid %in% iid) == length(bid))
        #
        # (The above works without signalling an error.  FYI, there were 231513 entries
        # in bid.)

        notificationId <- shiny::showNotification("Step 3/3: Computing \"Deep\" Argo index. This may take a minute or two.", type="message", duration=NULL)
        ID <- i[["ID"]]
        cycle <- i[["cycle"]]
        lon <- i[["longitude"]]
        lat <- i[["latitude"]]
        idBGC <- unique(iBGC[["ID"]])
        n <- length(ID)
        type <- rep("core", n)
        type[ID %in% idBGC] <- "bgc"
        ##> ## This method took 0.44s (DE 2021-01-11), whereas the next one
        ##> ## took 0.027s.
        ##> type[grepl("849|862|864", i@data$index$profiler_type)] <- "deep"
        type[("849" == i@data$index$profiler_type)] <- "deep"
        type[("862" == i@data$index$profiler_type)] <- "deep"
        type[("864" == i@data$index$profiler_type)] <- "deep"
        argo <- data.frame(time=i[["date"]], ID=ID, cycle=cycle, longitude=lon, latitude=lat, type=type)
        argo$longitude <- ifelse(argo$longitude > 180, argo$longitude - 360, argo$longitude)
        ok <- is.finite(argo$time)
        argo <- argo[ok, ]
        argoFloatsStoreInCache("argo", argo, debug=debug)
    }

    ok <- is.finite(argo$longitude)
    argo <- argo[ok, ]
    ok <- is.finite(argo$latitude)
    argo <- argo[ok, ]
    visible <- rep(TRUE, length(argo$lon)) # vector indicating whether to keep any given cycle.

    ## Functions used to prevent off-world points
    pinlat <- function(lat)
        ifelse(lat < -90, -90, ifelse(90 < lat, 90, lat))
    pinlon <- function(lon)
        ifelse(lon < -180, -180, ifelse(180 < lon, 180, lon))

    output$UIview <- shiny::renderUI({
        if (argoFloatsIsCached("argo") && input$tabselected %in% c(1)) {
            shiny::removeNotification(notificationId)
            #notificationIdDeep <- shiny::showNotification("Step 4/5: Creating widgets", type="message", duration=2)
            shiny::checkboxGroupInput("view",
                label="View",
                choiceNames=list(shiny::tags$span("Core",style=paste0('font-weight:bold; color:#',paste(as.raw(as.vector(col2rgb(ifelse(input$Ccolour == "default", colDefaults$core, input$Ccolour)))), collapse=''))),
                    shiny::tags$span("Deep",
                        style=paste0('font-weight:bold; color:#',paste(as.raw(as.vector(col2rgb(ifelse(input$Dcolour == "default", colDefaults$deep, input$Dcolour)))), collapse=''))),
                    shiny::tags$span("BGC",
                        style=paste0('font-weight:bold; color:#',paste(as.raw(as.vector(col2rgb(ifelse(input$Bcolour == "default", colDefaults$bgc, input$Bcolour)))), collapse=''))),
                    shiny::tags$span("HiRes", style="color: black;"),
                    shiny::tags$span("Topo", style="color: black;"),
                    shiny::tags$span("Path", style="color:black;"),
                    shiny::tags$span("Contour", style="color:black;")),
                choiceValues=list("core", "deep", "bgc", "hires", "topo", "path", "contour"),
                selected=state$view,
                inline=TRUE)
        }
    })

    output$UIwidget <- shiny::renderUI({
        if (argoFloatsIsCached("argo") && input$tabselected %in% c(1)) {
            shiny::fluidRow(shiny::span(shiny::HTML(paste("<b style=\"color:blue; margin-left:2em;\">  ",appName, "</b>"))),
                shiny::actionButton("help", "Help"),
                shiny::actionButton("code", "Code"),
                shiny::actionButton("goW", shiny::HTML("&larr;")),
                shiny::actionButton("goN", shiny::HTML("&uarr;")),
                shiny::actionButton("goS", shiny::HTML("&darr;")),
                shiny::actionButton("goE", shiny::HTML("&rarr;")),
                shiny::actionButton("zoomIn", "+"),
                shiny::actionButton("zoomOut", "-"),
                shiny::div(style="display: inline-block; vertical-align:center; width: 8em; margin: 0; padding-left:0px;",shiny::dateInput(inputId="start", label="Start", value=state$startTime)),
                shiny::div(style="display: inline-block;vertical-align:top; width: 8em;", shiny::dateInput(inputId="end", label="End", value=state$endTime)))
        }
    })

    output$UItrajectory <- shiny::renderUI({
        if (argoFloatsIsCached("argo") && input$tabselected %in% c(1) && "path" %in% state$view) {
            shiny::fluidRow(shiny::column(6,
                    style="padding-left:0px;",
                    shiny::checkboxGroupInput("action",
                        label="",
                        choiceNames=list(shiny::tags$span("Start", style="color: black;"),
                            shiny::tags$span("End", style="color: black;"),
                            shiny::tags$span("Without Profiles", style="color: black;")),
                        choiceValues=list( "start", "end", "lines"), selected=state$action,
                        inline=TRUE)))
        }
    })

    output$UIID <- shiny::renderUI({
        if (argoFloatsIsCached("argo") && input$tabselected %in% c(1)) {
            shiny::textInput("ID", "Float ID", value=state$focusID, width="8em")
        }
    })

    output$UIfocus <- shiny::renderUI({
        if (argoFloatsIsCached("argo") && input$tabselected %in% c(1)) {
            shiny::selectInput("focus", "Focus", choices=c("All"="all", "Single"="single"), selected=state$focus, width="10em")
        }
    })

    output$UIinfo <- shiny::renderUI({
        if (argoFloatsIsCached("argo")) {
            shiny::fluidRow(shiny::verbatimTextOutput("info"),
                if (state$hoverIsPasted == TRUE)
                    tags$head(tags$style("#info{color: red}"
                    )
                    )
            )
        }
    })

    output$info <- shiny::renderText({
        ## show location.  If lat range is under 90deg, also show nearest float within 100km
        if (state$hoverIsPasted)
            return(paste("[HOLD:ON] ",lastHoverMessage))
        x <- input$hover$x
        y <- input$hover$y
        if (is.null(x) && input$tabselected == 1)
            return("Hover mouse in plot to see locations; click-slide to select regions.")
        lonstring <- ifelse(x < 0, sprintf("%.2fW", abs(x)), sprintf("%.2fE", x))
        latstring <- ifelse(y < 0, sprintf("%.2fS", abs(y)), sprintf("%.2fN", y))
        rval <- ""
        if (diff(range(state$ylim)) < 90 && sum(visible)) {
            fac <- cos(y * pi180)      # account for meridional convergence
            dist2 <- ifelse(visible, (fac * (x - argo$longitude))^2 + (y - argo$latitude)^2, 1000)
            i <- which.min(dist2)
            dist <- sqrt(dist2[i]) * 111 # 1deg lat approx 111km
            if (length(dist) && dist < 100) {
                rval <- sprintf("%s %s, %.0f km from %s float %s cycle %s, at %s",
                    lonstring,
                    latstring,
                    dist,
                    switch(argo$type[i], "core"="Core", "bgc"="BGC", "deep"="Deep"),
                    argo$ID[i],
                    argo$cycle[i],
                    format(argo$time[i], "%Y-%m-%d %H:%M"))
            } else {
                rval <- sprintf("%s %s", lonstring, latstring)
            }
        } else {
            rval <- sprintf("%s %s", lonstring, latstring)
        }
        lastHoverMessage <<- rval
        rval
    })

    shiny::observeEvent(input$view,
        {
            state$view <<- input$view
        })

    shiny::observeEvent(input$action,
        {
            state$action <<- input$action
        }, ignoreNULL=FALSE)

    shiny::observeEvent(input$goE,
        {
            dx <- diff(state$xlim) # present longitude span
            state$xlim <<- pinlon(state$xlim + dx / 4)
        })

    shiny::observeEvent(input$goW,
        {
            dx <- diff(state$xlim) # present longitude span
            state$xlim <<- pinlon(state$xlim - dx / 4)
        })

    shiny::observeEvent(input$goS,
        {
            dy <- diff(state$ylim) # present latitude span
            state$ylim <<- pinlat(state$ylim - dy / 4)
        })

    shiny::observeEvent(input$goN,
        {
            dy <- diff(state$ylim) # present latitude span
            state$ylim <<- pinlat(state$ylim + dy / 4)
        })

    shiny::observeEvent(input$zoomIn,
        {
            state$xlim <<- pinlon(mean(state$xlim)) + c(-0.5, 0.5) / 1.3 * diff(state$xlim)
            state$ylim <<- pinlat(mean(state$ylim)) + c(-0.5, 0.5) / 1.3 * diff(state$ylim)
        })

    shiny::observeEvent(input$zoomOut,
        {
            state$xlim <<- pinlon(mean(state$xlim) + c(-0.5, 0.5) * 1.3 * diff(state$xlim))
            state$ylim <<- pinlat(mean(state$ylim) + c(-0.5, 0.5) * 1.3 * diff(state$ylim))
        })

    shiny::observeEvent(input$code,
        {
            msg <- "library(argoFloats)<br>"
            msg <- paste(msg, "# Download (or use cached) index from one of two international servers.<br>")
            if ("core" %in% state$view && "bgc" %in% state$view && "deep" != state$view) {
                msg <- paste(msg, "ai <- getIndex()<br>")
                msg <- paste(msg, "bai <- getIndex(filename=\"bgc\")<br>")
                msg <- paste(msg, "mai <- merge(ai,bai)<br>")
                msg <- paste(msg, "# Subset to remove deep profiles.<br>")
                msg <- paste(msg, "index <- subset(mai, deep=FALSE)<br>")
            } else if ("core" %in% state$view && "deep" %in% state$view && "bgc" != state$view) {
                msg <- paste(msg, "ai <- getIndex()<br>")
                msg <- paste(msg, "bai <- getIndex(filename=\"bgc\")<br>")
                msg <- paste(msg, "# Subset deep profiles.<br>")
                msg <- paste(msg, "deep1 <- subset(ai, deep=TRUE)<br>")
                msg <- paste(msg, "deep2 <- subset(bai, deep=TRUE)<br>")
                msg <- paste(msg, "deep <- merge(deep1,deep2)<br>")
                msg <- paste(msg, "index <- merge(ai,deep)<br>")
            } else if ("bgc" %in% state$view && "deep" %in% state$view && "core" != state$view) {
                msg <- paste(msg, "ai <- getIndex()<br>")
                msg <- paste(msg, "bai <- getIndex(filename=\"bgc\")<br>")
                msg <- paste(msg, "# Subset deep profiles.<br>")
                msg <- paste(msg, "deep1 <- subset(ai, deep=TRUE)<br>")
                msg <- paste(msg, "deep2 <- subset(bai, deep=TRUE)<br>")
                msg <- paste(msg, "deep <- merge(deep1,deep2)<br>")
                msg <- paste(msg, "index <- merge(bai,deep)<br>")
            } else if ("bgc" %in% state$view && "deep" %in% state$view && "core" %in% state$view) {
                msg <- paste(msg, "ai <- getIndex()<br>")
                msg <- paste(msg, "bai <- getIndex(filename=\"bgc\")<br>")
                msg <- paste(msg, "index <- merge(ai,bai)<br>")
            } else if ("core" %in% state$view && "bgc" != state$view && "deep" != state$view) {
                msg <- paste(msg, "index <- getIndex()<br>")
            } else if ("bgc" %in% state$view && "core" != state$view && "deep" != state$view) {
                msg <- paste(msg, "index <- getIndex(filename=\"bgc\")<br>")
            } else if ("deep" %in% state$view && "core" != state$view && "bgc" != state$view) {
                msg <- paste(msg, "bai <- getIndex(filename=\"bgc\")<br>")
                msg <- paste(msg, "ai <- getIndex()<br>")
                msg <- paste(msg, "# Subset deep profiles.<br>")
                msg <- paste(msg, "deep1 <- subset(ai, deep=TRUE)<br>")
                msg <- paste(msg, "deep2 <- subset(bai, deep=TRUE)<br>")
                msg <- paste(msg, "index <- merge(deep1,deep2)<br>")
            }
            msg <- paste(msg, "# Subset by time.<br>")
            msg <- paste(msg, "from <- as.POSIXct(\"", format(state$startTime, "%Y-%m-%d", tz="UTC"), "\", tz=\"UTC\")<br>", sep="")
            msg <- paste(msg, "to <- as.POSIXct(\"", format(state$endTime, "%Y-%m-%d", tz="UTC"), "\", tz=\"UTC\")<br>", sep="")
            msg <- paste(msg, "subset1 <- subset(index, time=list(from=from, to=to))<br>")
            msg <- paste(msg, "# Subset by space.<br>", sep="")
            lonRect <- state$xlim
            latRect <- state$ylim
            msg <- paste(msg, sprintf("rect <- list(longitude=c(%.4f,%.4f), latitude=c(%.4f,%.4f))<br>",
                                      lonRect[1], lonRect[2], latRect[1], latRect[2]))
            msg <- paste(msg, "subset2 <- subset(subset1, rectangle=rect)<br>")
            if ("single" %in% state$focus && nchar(state$focusID) > 0){
                msg <- paste0(msg, sprintf("subset2 <- subset(subset2, ID=%2s)<br>", state$focusID))
            }
            msg <- paste(msg, "# Plot a map (with different formatting than used here).<br>")
            if ("topo" %in% state$view) {
                if ("path" %in% state$view) {
                    if("lines" %in% state$action) {
                        msg <- paste(msg, "plot(subset2, which=\"map\", type=\"l\")<br>")
                    } else  {
                        msg <- paste(msg, "plot(subset2, which=\"map\", type=\"o\")<br>")
                    }
                } else {
                    msg <- paste(msg, "plot(subset2, which=\"map\")<br>")
                }
            } else {
                if ("path" %in% state$view) {
                    if ("lines" %in% state$action) {
                        msg <- paste(msg, "plot(subset2, which=\"map\", bathymetry=FALSE, type=\"l\")<br>")
                    } else {
                        msg <- paste(msg, "plot(subset2, which=\"map\", bathymetry=FALSE, type=\"o\")<br>")
                    }
                } else {
                    msg <- paste(msg, "plot(subset2, which=\"map\", bathymetry=FALSE)<br>")
                }
            }

            if ("start" %in% state$action) {
                msg <- paste(msg, "o <- order(subset2[['time']])<br>")
                msg <- paste(msg, "lat <- subset2[['latitude']]<br>")
                msg <- paste(msg, "lon <- subset2[['longitude']]<br>")
                msg <- paste(msg, "points(lon[1],lat[1], pch=2, cex=2, lwd=1.4)<br>")
            }

            if ("end" %in% state$action) {
                msg <- paste(msg, "o <- order(subset2[['time']])<br>")
                msg <- paste(msg, "lat <- subset2[['latitude']]<br>")
                msg <- paste(msg, "lon <- subset2[['longitude']]<br>")
                msg <- paste(msg, "no <- length(o)<br>")
                msg <- paste(msg, "points(lon[no],lat[no], pch=0, cex=2, lwd=1.4)<br>")
            }

            if ("contour" %in% state$view) {
                msg <- paste(msg, "# Adding contour. <br>")
                msg <- paste(msg, "data(topoWorld)<br>")
                msg <- paste(msg, "contour(topoWorld[[\"longitude\"]], topoWorld[[\"latitude\"]], topoWorld[[\"z\"]], levels=-1000*(1:10), drawlabels=FALSE, add=TRUE)<br>")
            }
            msg <- paste(msg, "# The following shows how to make a TS plot for these profiles. This<br>")
            msg <- paste(msg, "# involves downloading, which will be slow for a large number of<br>")
            msg <- paste(msg, "# profiles, so the steps are placed in an unexecuted block.<br>")
            msg <- paste(msg, "if (FALSE) {<br>")
            msg <- paste(msg, "&nbsp;&nbsp; profiles <- getProfiles(subset2)<br>")
            msg <- paste(msg, "&nbsp;&nbsp; argos <- readProfiles(profiles)<br>")
            msg <- paste( msg, "&nbsp;&nbsp; plot(applyQC(argos), which=\"TS\", TSControl=list(colByCycle=1:8))<br>")
            msg <- paste(msg, "}<br>")
            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title="R code", size="l"))
        })

    shiny::observeEvent(input$ID,
        {
            if (0 == nchar(input$ID)) {
                state$focusID <<- NULL
                shiny::updateTextInput(session, "focus", value="all")
            } else {
                k <- which(argo$ID == input$ID)
                if (length(k)) {
                    state$focusID <<- input$ID
                    state$xlim <<- pinlon(extendrange(argo$lon[k], f = 0.15))
                    state$ylim <<- pinlat(extendrange(argo$lat[k], f = 0.15))
                    if (state$focus == "all")
                        shiny::showNotification(paste0("Since you entered a float ID (", input$ID, "), you might want to change Focus to \"Single\""),
                            type="message", duration=10)
                } else {
                    shiny::showNotification(paste0("There is no float with ID ", input$ID, "."), type="error")
                }
            }
        })

    shiny::observeEvent(input$focus,
        {
            state$focus <<- input$focus
            if (input$focus == "single") {
                if (is.null(state$focusID)) {
                    shiny::showNotification(
                        "Double-click on a point or type an ID in the 'Float ID' box, to single out a focus float",
                        type = "error",
                        duration = NULL)
                } else {
                    state$focus <<- input$focus
                    k <- argo$ID == state$focusID
                    ## Extend the range 3X more than the default, because I almost always
                    ## end up typing "-" a few times to zoom out
                    state$xlim <<- pinlon(extendrange(argo$lon[k], f = 0.15))
                    state$ylim <<- pinlat(extendrange(argo$lat[k], f = 0.15))
                    ## Note: extending time range to avoid problems with day transitions,
                    ## which might cause missing cycles at the start and end; see
                    ## https://github.com/ArgoCanada/argoFloats/issues/283.
                    state$startTime <<- min(argo$time[k])
                    state$endTime <<- max(argo$time[k])
                    #message("setting box")
                    shiny::updateTextInput(session, "start",
                        value=format(state$startTime, "%Y-%m-%d"))
                    shiny::updateTextInput(session, "end",
                        value=format(state$endTime, "%Y-%m-%d"))
                }
            } else {
                # "all"
                shiny::updateTextInput(session, "ID", value="")
                state$focusID <<- NULL
                state$startTime <<- startTime
                state$endTime <<- endTime
                shiny::updateTextInput(session, "start",
                    value=format(state$startTime, "%Y-%m-%d"))
                shiny::updateTextInput(session, "end",
                    value=format(state$endTime, "%Y-%m-%d"))
            }
        })

    shiny::observeEvent(input$dblclick,
        {
            x <- input$dblclick$x
            y <- input$dblclick$y
            fac <- 1 / cos(y * pi / 180) ^ 2 # for deltaLon^2 compared with deltaLat^2
            if (state$focus == "single" && !is.null(state$focusID)) {
                keep <- argo$ID == state$focusID
            } else {
                ## Restrict search to the present time window
                keep <- state$startTime <= argo$time & argo$time <= state$endTime
            }
            i <- which.min(ifelse(keep, fac * (x - argo$longitude) ^ 2 + (y - argo$latitude)^2, 1000))
            state$focusID <<- argo$ID[i]
            shiny::updateTextInput(session, "ID", value=state$focusID)
            shiny::updateSelectInput(session, "focus", selected="single")
            msg <- sprintf("ID %s, cycle %s<br>%s %.3fE %.3fN",
                argo$ID[i],
                argo$cycle[i],
                format(argo$time[i], "%Y-%m-%d"),
                argo$longitude[i],
                argo$latitude[i])
            shiny::showNotification(shiny::HTML(msg), duration=NULL)
        })

    shiny::observeEvent(input$start,
        {
            if (0 == nchar(input$start)) {
                state$startTime <<- min(argo$time, na.rm=TRUE)
            } else {
                t <- try(as.POSIXct(format(input$start, "%Y-%m-%d 00:00:00"), tz="UTC"), silent=TRUE)
                if (inherits(t, "try-error")) {
                    shiny::showNotification(paste0("Start time \"",
                            input$start,
                            "\" is not in yyyy-mm-dd format, or is otherwise invalid."), type="error")
                } else {
                    state$startTime <<- t
                    argoFloatsDebug(debug, "User selected start time ", format(t, "%Y-%m-%d %H:%M:%S %z"), "\n")
                }
            }
        })

    shiny::observeEvent(input$end,
        {
            if (0 == nchar(input$end)) {
                state$endTime <<- max(argo$time, na.rm = TRUE)
            } else {
                t <- try(as.POSIXct(format(input$end, "%Y-%m-%d 00:00:00"), tz="UTC"), silent=TRUE)
                if (inherits(t, "try-error")) {
                    shiny::showNotification(paste0( "End time \"", input$end, "\" is not in yyyy-mm-dd format, or is otherwise invalid."), type = "error")
                } else {
                    state$endTime <<- t
                    argoFloatsDebug(debug, "User selected end time ", format(t, "%Y-%m-%d %H:%M:%S %z"), "\n")
                }
            }
        })

    shiny::observeEvent(input$help,
        {
            msg <- shiny::HTML(overallHelp)
            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title="Using this application", size="l"))
        })

    shiny::observeEvent(input$CsymbolGallery,
        {
            shiny::showModal(shiny::modalDialog(shiny::img(src="/pch_gallery.png"),
                    title="Symbol Gallery", size="s", easyClose=TRUE))
        })

    shiny::observeEvent(input$BsymbolGallery,
        {
            shiny::showModal(shiny::modalDialog(shiny::img(src="/pch_gallery.png"),
                    title="Symbol Gallery", size="s", easyClose=TRUE))
        })

    shiny::observeEvent(input$DsymbolGallery,
        {
            shiny::showModal(shiny::modalDialog(shiny::img(src="/pch_gallery.png"),
                    title="Symbol Gallery", size="s", easyClose=TRUE))
        })

    shiny::observeEvent(input$keypressTrigger,
        {
            key <- intToUtf8(input$keypress)
            #message(input$keypress)
            #message(key)
            if (key == "n") { # go north
                dy <- diff(state$ylim)
                state$ylim <<- pinlat(state$ylim + dy / 4)
            } else if (key == "s") { # go south
                dy <- diff(state$ylim)
                state$ylim <<- pinlat(state$ylim - dy / 4)
            } else if (key == "e") { # go east
                dx <- diff(state$xlim)
                state$xlim <<- pinlon(state$xlim + dx / 4)
            } else if (key == "w") { # go west
                dx <- diff(state$xlim)
                state$xlim <<- pinlon(state$xlim - dx / 4)
            } else if (key == "f") { # forward in time
                interval <- as.numeric(state$endTime) - as.numeric(state$startTime)
                state$startTime <<- state$startTime + interval
                state$endTime <<- state$endTime + interval
                shiny::updateTextInput(session, "start", value=format(state$startTime, "%Y-%m-%d"))
                shiny::updateTextInput(session, "end", value=format(state$endTime, "%Y-%m-%d"))
            } else if (key == "b") { # backward in time
                interval <- as.numeric(state$endTime) - as.numeric(state$startTime)
                state$startTime <<- state$startTime - interval
                state$endTime <<- state$endTime - interval
                shiny::updateTextInput(session, "start", value=format(state$startTime, "%Y-%m-%d"))
                shiny::updateTextInput(session, "end", value=format(state$endTime, "%Y-%m-%d"))
            } else if (key == "i") { # zoom in
                state$xlim <<- pinlon(mean(state$xlim)) + c(-0.5, 0.5) / 1.3 * diff(state$xlim)
                state$ylim <<- pinlat(mean(state$ylim)) + c(-0.5, 0.5) / 1.3 * diff(state$ylim)
            } else if (key == "o") { # zoom out
                state$xlim <<- pinlon(mean(state$xlim) + c(-0.5, 0.5) * 1.3 * diff(state$xlim))
                state$ylim <<- pinlat(mean(state$ylim) + c(-0.5, 0.5) * 1.3 * diff(state$ylim))
            } else if (key == "h") { # paste hover message
                state$hoverIsPasted <<- !state$hoverIsPasted
            } else if (key == "r") { # reset to start
                state$xlim <<- c(-180, 180)
                state$ylim <<- c(-90, 90)
                state$startTime <<- startTime
                state$endTime <<- endTime
                state$focusID <<- NULL
                state$hoverIsPasted <<- FALSE
                shiny::updateSelectInput(session, "focus", selected="all")
                shiny::updateCheckboxGroupInput(session, "show", selected=character(0))
                shiny::updateDateInput(session, inputId="start", label="Start", value=startTime)
                shiny::updateDateInput(session, inputId="end", label="End", value=endTime)
                state$view <<- c("core", "deep", "bgc")
                state$action <<- NULL
            } else if (key == "0") { # Unzoom an area and keep same time scale
                state$xlim <<- c(-180, 180)
                state$ylim <<- c(-90, 90)
                shiny::updateSelectInput(session, "focus", selected="all")
                    shine::updateCheckboxGroupInput(session, "show", selected=character(0))
                } else if (key == "?") { # show help on keystrokes
                shiny::showModal(shiny::modalDialog(title="Key-stroke commands",
                        shiny::HTML(keyPressHelp), easyClose=TRUE))
            }
        })                                  # keypressTrigger

    output$plotMap <- shiny::renderPlot({
        if (state$startTime > state$endTime) {
            shiny::showNotification(
                paste0("Start must precede End, but got Start=",
                    format(state$startTime, "%Y-%m-%d"), " and End=",
                    format(state$endTime, "%Y-%m-%d.")), type="error")
        } else {
            if (!is.null(input$brush)) {
                ## Require a minimum size, to avoid mixups with minor click-slide
                if ((input$brush$xmax - input$brush$xmin) > 0.5 && (input$brush$ymax - input$brush$ymin) > 0.5) {
                    state$xlim <<- c(input$brush$xmin, input$brush$xmax)
                    state$ylim <<- c(input$brush$ymin, input$brush$ymax)
                }
            }
            topo <- if ("hires" %in% state$view) topoWorldFine else topoWorld
            #notificationId <- shiny::showNotification("Step 5/5: Creating plot", type="message", duration=2)
            oldpar <- par(no.readonly=TRUE)
            par(mar=c(2.5, 2.5, 2, 1.5))
            on.exit(par(oldpar)
            plot(state$xlim, state$ylim, xlab="", ylab="", axes=FALSE, type="n", asp=1 / cos(pi / 180 * mean(state$ylim)))
            if ("topo" %in% state$view) {
                image(topo[["longitude"]], topo[["latitude"]], topo[["z"]], add=TRUE, breaks=seq(-8000, 0, 100), col=oce::oceColorsGebco(80))
            }
            usr <- par("usr")
            usr[1] <- pinlon(usr[1])
            usr[2] <- pinlon(usr[2])
            usr[3] <- pinlat(usr[3])
            usr[4] <- pinlat(usr[4])
            at <- pretty(usr[1:2], 10)
            at <- at[usr[1] < at & at < usr[2]]
            labels <- paste(abs(at), ifelse(at < 0, "W", ifelse(at > 0, "E", "")), sep="")
            axis(1, pos=pinlat(usr[3]), at=at, labels=labels, lwd=1, cex.axis=0.8)
            at <- pretty(usr[3:4], 10)
            at <- at[usr[3] < at & at < usr[4]]
            labels <- paste(abs(at), ifelse(at < 0, "S", ifelse(at > 0, "N", "")), sep="")
            axis(2, pos=pinlon(usr[1]), at=at, labels=labels, lwd=1, cex.axis=0.8)
            coastline <- if ("hires" %in% state$view) coastlineWorldFine else coastlineWorld
            polygon(coastline[["longitude"]], coastline[["latitude"]], col=colLand)
            rect(usr[1], usr[3], usr[2], usr[4], lwd = 1)
            ## For focusID mode, we do not trim by time or space
            keep <- if (state$focus == "single" && !is.null(state$focusID)) {
                argo$ID == state$focusID
            }  else {
                rep(TRUE, length(argo$ID))
            }
            argoFloatsDebug(debug, "about to subset, start time = ", format(state$startTime, "%Y-%m-%d %H:%M:%S %z"), "\n")
            argoFloatsDebug(debug, "about to subset, end time = ", format(state$endTime, "%Y-%m-%d %H:%M:%S %z"), "\n")
            keep <- keep & (state$startTime <= argo$time & argo$time <= state$endTime)
            keep <- keep & (state$xlim[1] <= argo$longitude & argo$longitude <= state$xlim[2])
            keep <- keep & (state$ylim[1] <= argo$latitude & argo$latitude <= state$ylim[2])
            cex <- 0.9
            if (sum(c("core", "deep", "bgc") %in% state$view) > 0) {
                ## Draw points, optionally connecting paths (and indicating start points)
                visible <<- rep(FALSE, length(argo$lon))
                for (view in c("core", "deep", "bgc")) {
                    if (view %in% state$view) {
                        k <- keep & argo$type == view
                        visible <<- visible | k
                        lonlat <- argo[k,]
                        argoFloatsDebug(debug, "view= ", view, " , sum(k)= ", sum(k), ", length(k)= ",length(k),"\n")
                        colSettings <- list(core=if (input$Ccolour == "default") colDefaults$core else input$Ccolour,
                            bgc=if (input$Bcolour == "default") colDefaults$bgc else input$Bcolour,
                            deep=if (input$Dcolour == "default") colDefaults$deep else input$Dcolour)
                        symbSettings <- list(core=input$Csymbol, bgc=input$Bsymbol, deep=input$Dsymbol)
                        borderSettings <- list(core=input$Cborder, bgc=input$Bborder, deep=input$Dborder)
                        sizeSettings <- list(core=input$Csize, bgc=input$Bsize, deep=input$Dsize)
                        #message("the symbSettings are", symbSettings)
                        #message("the colSettings are", colSettings)
                        if (!"lines" %in% state$action)
                            if (symbSettings[[view]] == 21) {
                                points(lonlat$lon, lonlat$lat, pch=symbSettings[[view]], cex=sizeSettings[[view]], bg=colSettings[[view]], col=borderSettings[[view]], lwd=0.5)
                            } else {
                                points(lonlat$lon, lonlat$lat, pch=symbSettings[[view]], cex=sizeSettings[[view]], col=colSettings[[view]], bg=colSettings[[view]], lwd=0.5)
                            }
                        if ("path" %in% state$view) {
                            for (ID in unique(lonlat$ID)) {
                                LONLAT <- lonlat[lonlat$ID==ID,]
                                ## Sort by time instead of relying on the order in the repository
                                o <- order(LONLAT$time)
                                no <- length(o)
                                if (no > 1) {
                                    #message("view = '", view, "' jaimie")
                                    pathColour <- list(core=if (input$CPcolour == "default") colDefaults$core else input$CPcolour,
                                        bgc=if (input$BPcolour == "default") colDefaults$bgc else input$BPcolour,
                                        deep=if (input$DPcolour == "default") colDefaults$deep else input$DPcolour)
                                    pathWidth <- list(core=input$CPwidth, bgc=input$BPwidth, deep=input$DPwidth)
                                    LONLAT <<- LONLAT[o, ]
                                    #message(pathColour[[view]], " is the path color")
                                    lines(LONLAT$lon, LONLAT$lat, lwd=pathWidth[[view]], col=pathColour[[view]])
                                    ## as opposed to maybe 3 months of data for a set of floats).
                                    if ("start" %in% state$action)
                                        points(LONLAT$lon[1], LONLAT$lat[1], pch=2, cex=1, lwd=1.4)
                                    if ("end" %in% state$action)
                                        points(LONLAT$lon[no], LONLAT$lat[no], pch=0, cex=1, lwd=1.4)
                                }
                            }
                        }
                        if ("contour" %in% state$view){
                            contour(topo[["longitude"]], topo[["latitude"]], topo[["z"]], levels=-1000*(1:10), drawlabels=FALSE, add=TRUE)
                        }
                    }
                }
                ## Draw the inspection rectangle as a thick gray line, but only if zoomed
                if (-180 < state$xlim[1] || state$xlim[2] < 180 || -90 < state$ylim[1] || state$ylim[2] < 90)
                    rect(state$xlim[1], state$ylim[1], state$xlim[2], state$ylim[2], border="darkgray", lwd=4)
                ## Write a margin comment
                if (state$focus == "single" && !is.null(state$focusID)) {
                    mtext(sprintf("Float %s: %s to %s",
                            state$focusID,
                            format(state$startTime, "%Y-%m-%d", tz="UTC"),
                            format(state$endTime, "%Y-%m-%d", tz="UTC")),
                        side=3, cex=0.8 * par("cex"), line=0.25)
                } else {
                    mtext(sprintf("%s to %s: %d Argo profiles",
                            format(state$startTime, "%Y-%m-%d", tz="UTC"),
                            format(state$endTime, "%Y-%m-%d", tz="UTC"),
                            sum(visible)),
                        side=3, line=0.25, cex=0.8 * par("cex"))
                    if (diff(range(state$ylim)) < 90 && sum(visible)) {
                        oce::mapScalebar(x="topright") }
                }
            }                          # if (sum(c("core", "deep", "bgc") %in% state$view) > 0)
        }
    }, height=500, pointsize=18)       # plotMap
}                                      # serverMapApp

shiny::shinyApp(ui=uiMapApp, server=serverMapApp)

