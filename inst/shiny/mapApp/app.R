# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

plotCounter <- 1L

# Set debug to FALSE to turn off these program-flow messages (although some are also
# controlled by 'debug' passed as an argument to functions).
debug <- 1L
dmsg <- function(...)
{
    if (debug)
        message(...)
}

viewDefaults <- list("core", "deep", "bgc")
colDefaults <- list(core="#F5C710", bgc="#05f076", deep="#CD0BBC")

# Default start and end times. We must extend present time because
# it gets stored as a date, so otherwise we miss all profiles taken "today".
dayStart <- function(t)
{
    t <- as.POSIXlt(t, tz="UTC")
    t$hour <- 0L
    t$min <- 0L
    t$sec <- 0.0
    t
}
dayEnd <- function(t)
{
    t <- as.POSIXlt(as.POSIXct(t, tz="UTC") + 86400, tz="UTC")
    t$hour <- 0L
    t$min <- 0L
    t$sec <- 0.0
    t
}
secondsPerDay <- 86400
now <- Sys.time()
endTime <- dayEnd(now)
startTime <- dayStart(now - 10 * secondsPerDay)
#checking dput(now)
#checking dput(startTime)
#checking dput(endTime)
#checking endTime-startTime

stateStack <- list()
pushState <- function(state, debug=0L)
{
    argoFloatsDebug(debug, "pushState() { \n", style="bold", unindent=1)
    nss <- sizeState()
    if (nss == 0L || !identical(state, stateStack[[nss]])) {
        stateStack[[nss + 1L]] <<- state
        argoFloatsDebug(debug, "pushed state, yielding a new stack length of", nss+1, "\n")
        if (debug > 0L)
            printState()
    } else {
        argoFloatsDebug(debug, "ignoring duplicate state, retaining old stack of length", nss, "\n")
    }
    argoFloatsDebug(debug, "} # pushState()\n", style="bold", unindent=1)
}

popState <- function(debug=0L)
{
    argoFloatsDebug(debug, "popState() {\n", style="bold", unindent=1)
    nss <- sizeState()
    if (nss > 1L) {
        stateStack[[nss]] <<- NULL
        argoFloatsDebug(debug, "reduced stack length to", nss-1L, "\n")
    } else {
        argoFloatsDebug(debug, "leaving stack alone, because it is already empty\n")
    }
    #printState()
    argoFloatsDebug(debug, "} # popState()\n", style="bold", unindent=1)
}

topState <- function()
{
    argoFloatsDebug(debug, "topState() {\n", style="bold", unindent=1)
    argoFloatsDebug(debug, "stack length=", length(stateStack), "\n", sep="")
    nss <- sizeState()
    argoFloatsDebug(debug, "} # topState()\n\n", style="bold", unindent=1)
    if (nss > 0L) stateStack[[nss]] else NULL
}
sizeState <- function()
{
    length(stateStack)
}
printState <- function(debug=0L)
{
    argoFloatsDebug(debug, "printState() {\n\n", style="bold", unindent=1)
    if (debug) {
        nss <- sizeState()
        if (nss > 0L) {
            argoFloatsDebug(debug , "stateStack holds", nss, "elements\n")
            if (FALSE) {
                argoFloatsDebug(debug, "top element:\n")
                topOfStack <- stateStack[[nss]]
                for (name in sort(names(topOfStack)))
                    argoFloatsDebug(debug, name, ": ",format(paste(topOfStack[[name]], collapse=" ")))
            }
        } else {
            argoFloatsDebug(debug, "stateStack is empty\n")
        }
    } else {
        argoFloatsDebug(debug, "skipping printing since debug=", debug, "\n")
    }
    argoFloatsDebug(debug, "} # printState()\n\n", style="bold", unindent=1)
}


pi180 <- pi / 180                      # degree/radian conversion factor

keyPressHelp <- "<ul>
<li> '<b>d</b>': toggle <b>d</b>ebugging flag</li>
<li> '<b>h</b>': hold active hover message (press <b>h</b> again to undo)</li>
<li> '<b>r</b>': <b>r</b>eset to initial state</li>
<li> '<b>q</b>': indicate when finished polygon</li>
<li> '<b>u</b>': <b>u</b>ndo previous actions</li>
<li> '<b>0</b>': unzoom an area</li>
<li> '<b>?</b>': display this message</li>
</ul>"

overallHelp <- "<p>mapApp() responds to keystroke actions and GUI actions.</p><p>The permitted <u>keystroke actions</u> will be shown in a pop-up window if the <b>?</b> key is pressed. There are keys for zooming in and out, for moving the focus region through space and time, for controlling updates to an information box that displays mouse location and aspects of a nearby float, undoing previous actions, and turning on a developer mode in which information about processing is printed to the R console.</p><p>The <u>GUI actions</u> are reasonably self-explanatory. On the <i>Map tab</i>, users may enter values in the \"Start\" and \"End\" boxes to set the time range of the display, or empty either box to use the data range. The checkboxes of the \"View\" grouping may be used to choose whether to show 'Core', 'Deep' or 'BGC' data, whether to draw a high-resolution coastline, whether to draw connecting line segments to indicate the path of individual floats, and whether to indicate water depth using contour lines. If a path is displayed, there are options to highlight its start and end points of the path in the selected region, or to hide all points. The focus region may be selected by pressing the mouse at one location, sliding it to a new location, and then releasing it. Double-clicking on a particular float location creates a pop-up window that provides information on that profile. There is a way to focus on an individual float, to the exclusion of others.  Experimenting with the interface will reveal other capabilities; for example, it is worth exploring the <i>Settings tab</i>, which provides control over several aesthetic properties.<p>A text box above the plot shows the mouse position in longitude and latitude as well as information about the nearest profile, if it is within 100km of the mouse location (typing <b>h</b> toggles a setting that causes this information to track the mouse).</p><p>The \"Code\" button brings up a window showing R code that will approximate the view shown in the app, and that hints at some other operations that might be useful in analysis.</p><p>For more details, type <tt>?argoFloats::mapApp</tt> in an R console.</p>"


uiMapApp <- shiny::fluidPage(
    # Header Panel
    shiny::headerPanel(title="", windowTitle="argoFloats mapApp"),
    shiny::tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    #style="text-indent:1em; line-height:1.2; background:#e6f3ff; .btn{ padding: 2px 9px; }; .form-group { margin-top: 0; margin-bottom: 0 }",
    # margin-top works, but not sure if either pre{} or formgroup{} work.
    style="text-indent:1em; line-height:1.2; background:#e6f3ff; margin-top: -2ex; pre { line-height: 0.5; }; .form-group { margin-top: 3px; margin-bottom: 3px;};",
    shiny::fluidRow(
        shiny::uiOutput(outputId="UIwidget1")),
    shiny::fluidRow(
        shiny::uiOutput(outputId="UIwidget2")),
    shiny::fluidRow(
        shiny::column(9, shiny::uiOutput(outputId="UIview"))),
    shiny::fluidRow(
        shiny::uiOutput(outputId="UItrajectory")),
    shiny::fluidRow(
        shiny::uiOutput(outputId="UIinfo")),

    # Main Panel
    shiny::mainPanel(shiny::tabsetPanel(type="tab",
            shiny::tabPanel("Map", value=1),
            shiny::tabPanel("Settings", value=2,
                shiny::tabsetPanel(shiny::tabPanel("Core", value=3, selected=TRUE),
                    shiny::tabPanel("BGC", value=4),
                    shiny::tabPanel("Deep", value=5),
                    id="settab")),
            id="tabselected"),
        shiny::uiOutput(outputId="UIcoreTab"),
        shiny::uiOutput(outputId="UIbgcTab"),
        shiny::uiOutput(outputId="UIdeepTab")),

    shiny::conditionalPanel("input.tabselected!=2",
        shiny::fluidRow(shiny::plotOutput("plotMap",
                hover=shiny::hoverOpts("hover"),
                dblclick=shiny::dblclickOpts("dblclick"),
                click=shiny::clickOpts("click"),
                brush=shiny::brushOpts("brush", delay=2000, resetOnNew=TRUE)))))

serverMapApp <- function(input, output, session)
{
    lastHoverMessage <- "" # used with 'h' keystroke
    age <- shiny::getShinyOption("age")
    destdir <- shiny::getShinyOption("destdir")
    argoServer <- shiny::getShinyOption("argoServer")
    colLand <- shiny::getShinyOption("colLand")
    debug <<- shiny::getShinyOption("debug")
    if (!requireNamespace("shiny", quietly=TRUE))
        stop("must install.packages('shiny') for mapApp() to work")
    if (!requireNamespace("colourpicker", quietly=TRUE))
        stop("must install.packages('colourpicker') for mapApp() to work")
    # State: reactive
    state <- shiny::reactiveValues(
        begin=TRUE,
        xlim=c(-180, 180),
        ylim=c(-90, 90),
        polyDone=FALSE,
        polygon=FALSE,
        data=NULL,
        startTime=startTime,
        endTime=endTime,
        action=NULL,
        focusID=NULL,
        view=viewDefaults,
        Ccolour=colDefaults$core,
        Csymbol=21,
        Csize=0.9,
        Cborder="black",
        CPcolour=colDefaults$core,
        CPwidth=1.4,
        Bcolour=colDefaults$bgc,
        Bsymbol=21,
        Bsize=0.9,
        Bborder="black",
        BPcolour=colDefaults$bgc,
        BPwidth=1.4,
        Dcolour=colDefaults$deep,
        Dsymbol=21,
        Dsize=0.9,
        Dborder="black",
        DPcolour=colDefaults$deep,
        DPwidth=1.4,
        hoverIsPasted=FALSE)
    pushState(isolate(reactiveValuesToList(state)))
    # Depending on whether 'hires' selected, 'coastline' will be one of the following two version:
    data("coastlineWorld", package="oce", envir=environment())
    coastlineWorld <- get("coastlineWorld")
    data("coastlineWorldFine", package="ocedata", envir=environment())
    coastlineWorldFine <- get("coastlineWorldFine")
    # Depending on whether 'hires' selected, 'topo' wil be one of the following two version:
    data("topoWorld", package="oce", envir=environment())
    topoWorld <- get("topoWorld")
    if (file.exists("topoWorldFine.rda")) {
        load("topoWorldFine.rda")
    } else {
        topoWorldFine <- topoWorld
    }
    argoFloatsDebug(debug, "mapApp() is about to check the cache\n")
    if (argoFloatsIsCached("argo", debug=debug-1L)) {
        notificationId <- shiny::showNotification("Using argo data that were cached temporarily in R-session memory\n")
        argo <- argoFloats::argoFloatsGetFromCache("argo", debug=debug-1L)
    } else {
        # Get core and BGC data.
        notificationId <- shiny::showNotification("Step 1/3: Getting \"core\" Argo index, either by downloading new data or using data in \"destdir\".  This may take a minute or two.", type="message", duration=NULL)
        i <<- argoFloats::getIndex(age=age, destdir=destdir, server=argoServer, debug=debug)
        argoFloatsDebug(debug, "getIndex() returned", if (is.null(i)) "NULL" else "not NULL", "\n")
        shiny::removeNotification(notificationId)
        notificationId <- shiny::showNotification("Step 2/3: Getting \"BGC\" Argo index, either by downloading new data or using cached data.  This may take a minute or two.", type="message", duration=NULL)
        iBGC <<- argoFloats::getIndex("bgc", age=age, destdir=destdir, server=argoServer, debug=debug)
        m <<- merge(i, iBGC)
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
        profilerType <- i[["profiler_type"]]
        institution <- i[["institution"]]
        file <- i[["file"]]
        idBGC <- unique(iBGC[["ID"]])
        n <- length(ID)
        type <- rep("core", n)
        type[ID %in% idBGC] <- "bgc"
        #> ## This method took 0.44s (DE 2021-01-11), whereas the next one
        #> ## took 0.027s.
        #> type[grepl("849|862|864", i@data$index$profiler_type)] <- "deep"
        type[("849" == i@data$index$profiler_type)] <- "deep"
        type[("862" == i@data$index$profiler_type)] <- "deep"
        type[("864" == i@data$index$profiler_type)] <- "deep"
        argo <- data.frame(time=i[["date"]], ID=ID, cycle=cycle, longitude=lon, latitude=lat, type=type, profilerType=profilerType, institution=institution, file=file)
        argo$longitude <- ifelse(argo$longitude > 180, argo$longitude - 360, argo$longitude)
        ok <- is.finite(argo$time)
        argo <- argo[ok, ]
        argoFloatsStoreInCache("argo", argo, debug=debug-1L)
    }

    ok <- is.finite(argo$longitude)
    argo <- argo[ok, ]
    ok <- is.finite(argo$latitude)
    argo <- argo[ok, ]
    visible <- rep(TRUE, length(argo$lon)) # vector indicating whether to keep any given cycle.

    # Functions used to prevent off-world points
    pinlat <- function(lat)
        ifelse(lat < -90, -90, ifelse(90 < lat, 90, lat))
    pinlon <- function(lon)
        ifelse(lon < -180, -180, ifelse(180 < lon, 180, lon))

    output$UIview <- shiny::renderUI({
        if (argoFloatsIsCached("argo", debug=debug-1L) && input$tabselected %in% c(1)) {
            shiny::removeNotification(notificationId)
            shiny::checkboxGroupInput("view",
                label="View",
                choiceNames=list(
                    shiny::tags$span("Core",style=paste0('font-weight:bold; color:#',
                            paste(as.raw(as.vector(col2rgb(ifelse(state$Ccolour == "default",
                                                colDefaults$core,
                                                state$Ccolour)))),
                                collapse=""))),
                    shiny::tags$span("Deep",
                        style=paste0('font-weight:bold; color:#',
                            paste(as.raw(as.vector(col2rgb(ifelse(state$Dcolour == "default",
                                                colDefaults$deep,
                                                state$Dcolour)))),
                                collapse=""))),
                    shiny::tags$span("BGC",
                        style=paste0('font-weight:bold; color:#',
                            paste(as.raw(as.vector(col2rgb(ifelse(state$Bcolour == "default",
                                                colDefaults$bgc,
                                                state$Bcolour)))),
                                collapse=""))),
                    shiny::tags$span("HiRes", style="color: black;"),
                    shiny::tags$span("Topo", style="color: black;"),
                    shiny::tags$span("Contour", style="color:black;"),
                    shiny::tags$span("Path", style="color:black;")),
                choiceValues=list("core", "deep", "bgc", "hires", "topo", "contour", "path"),
                selected=state$view,
                #? selected=viewDefaults,
                inline=TRUE)
        }
    })

    output$UIcoreTab <- shiny::renderUI({
        if (input$tabselected %in% c(2) && input$settab %in% c(3)) {
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
                        colourpicker::colourInput("Ccolour", "Colour", state$Ccolour)),
                    shiny::column(2, shiny::numericInput("Csymbol", "Type", value=state$Csymbol, min=0, max=25)),
                    shiny::column(3, shiny::sliderInput("Csize", "Size", min=0, max=1, value=state$Csize, step=0.05))),
                shiny::fluidRow(
                    if (state$Csymbol == 21) {
                        shiny::column(3,
                            colourpicker::colourInput("Cborder", "Border Colour", value=state$Cborder))}),
                shiny::fluidRow(
                    shiny::div(style="color:black; font-weight:bold; margin-bottom: 10px;",
                        hr("Path Properties"))),
                shiny::fluidRow(
                    shiny::column(3,
                        colourpicker::colourInput("CPcolour", "Colour", state$CPcolour)),
                    shiny::column(3, shiny::sliderInput("CPwidth", "Width", min=0.5, max=6, value=state$CPwidth, step=0.5))))
        }
    })

    output$UIbgcTab <- shiny::renderUI({
        if (input$tabselected %in% c(2) && input$settab %in% c(4)) {
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
                        colourpicker::colourInput("Bcolour", "Colour", state$Bcolour)),
                    shiny::column(2, shiny::numericInput("Bsymbol", "Type", value=state$Bsymbol, min=0, max=25)),
                    shiny::column(3, shiny::sliderInput("Bsize", "Size", min=0, max=1, value=state$Bsize, step=0.05))),
                shiny::fluidRow(
                    if (state$Bsymbol == 21) {
                        shiny::column(3,
                            colourpicker::colourInput("Bborder", "Border Colour", value=state$Bborder))}),
                    shiny::fluidRow(
                    shiny::div(style="color:black; font-weight:bold; margin-bottom: 10px;",
                        hr("Path Properties"))),
                shiny::fluidRow(
                    shiny::column(3,
                        colourpicker::colourInput("BPcolour", "Colour", state$BPcolour)),
                    shiny::column(3, shiny::sliderInput("BPwidth", "Width", min=0.5, max=6, value=state$BPwidth, step=0.5))))
        }
    })

    output$UIdeepTab <- shiny::renderUI({
        if (input$tabselected %in% c(2) && input$settab %in% c(5)) {
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
                    colourpicker::colourInput("Dcolour", "Colour", state$Dcolour)),
                shiny::column(2, shiny::numericInput("Dsymbol", "Type", value=state$Dsymbol, min=0, max=25)),
                shiny::column(3, shiny::sliderInput("Dsize", "Size", min=0, max=1, value=state$Dsize, step=0.05))),
            shiny::fluidRow(
                if (state$Dsymbol == 21) {
                    shiny::column(3,
                        colourpicker::colourInput("Dborder", "Border Colour", value=state$Dborder))
                }),
            shiny::fluidRow(
                shiny::div(style="color:black; font-weight:bold; margin-bottom: 10px;",
                    hr("Path Properties"))),
            shiny::fluidRow(
                shiny::column(3,
                    colourpicker::colourInput("DPcolour", "Colour", state$DPcolour)),
                shiny::column(3, shiny::sliderInput("DPwidth", "Width", min=0.5, max=6, value=state$DPwidth, step=0.5))))
        }
    })

    output$UIwidget1 <- shiny::renderUI({
        if (argoFloatsIsCached("argo", debug=debug-1L) && input$tabselected %in% c(1)) {
            shiny::fluidRow(
                shiny::actionButton("help", "Help"),
                shiny::actionButton("undo", "Undo"),
                shiny::actionButton("code", "Code"),
                shiny::actionButton("goW", shiny::HTML("&larr;")),
                shiny::actionButton("goN", shiny::HTML("&uarr;")),
                shiny::actionButton("goS", shiny::HTML("&darr;")),
                shiny::actionButton("goE", shiny::HTML("&rarr;")),
                shiny::actionButton("zoomIn", "+"),
                shiny::actionButton("zoomOut", "-"),
                shinyBS::bsButton("polygon", shiny::HTML("&#x2B21;")),
                shinyBS::bsTooltip(id="polygon",title="To subset by polygon 1) Click this button 2) Click at least 3 points in the map 3) Click q to indicate done.", trigger="hover"),
                style="margin-left:0.5em;")
        }
    })
    output$UIwidget2 <- shiny::renderUI({
        if (argoFloatsIsCached("argo", debug=debug-1L) && input$tabselected %in% c(1)) {
            shiny::fluidRow(
                shiny::div(style="display: inline-block; vertical-align:center; width: 8em; margin: 0; padding-left:0px;",
                    shiny::dateInput(inputId="start", label="Start", value=state$startTime)),
                shiny::div(style="display: inline-block;vertical-align:top; width: 8em;",
                    shiny::dateInput(inputId="end", label="End", value=state$endTime)),
                shiny::div(style="display: inline-block;vertical-align:top; width: 8em;",
                    shiny::textInput("ID", "Float ID", value=state$focusID, width="8em")),
                style="margin-left:0.5em;")
        }
    })
    output$UItrajectory <- shiny::renderUI({
        if (argoFloatsIsCached("argo", debug=debug-1L) && input$tabselected %in% c(1) && "path" %in% state$view) {
            shiny::fluidRow(shiny::column(6,
                    style="text-indent:1em;",
                    shiny::checkboxGroupInput("action",
                        label="Path Properties",
                        choiceNames=list(shiny::tags$span("Start", style="color: black;"),
                            shiny::tags$span("End", style="color: black;"),
                            shiny::tags$span("Without Profiles", style="color: black;")),
                        choiceValues=list( "start", "end", "lines"), selected=state$action,
                        inline=TRUE)))
        }
    })
    output$UIinfo <- shiny::renderUI({
        if (argoFloatsIsCached("argo", debug=debug-1L)) {
            shiny::fluidRow(shiny::verbatimTextOutput("info"),
                if (state$hoverIsPasted) {
                    tags$head(tags$style("#info{color: red}"))
                } else {
                    tags$head(tags$style("#info{color: black}"))
                }
            )
        }
    })

    output$info <- shiny::renderText({
        # show location.  If lat range is under 90deg, also show nearest float within 100km
        if (input$tabselected == 1) {
            if (state$hoverIsPasted)
                return(paste("[HOLD]",lastHoverMessage))
        x <- input$hover$x
        y <- input$hover$y
        if (is.null(x) && input$tabselected == 1)
            return("Hover to see location/cycle; brush to select region; double-click to restrict ID.")
        rval <- ""
        if (diff(range(state$ylim)) < 90 && sum(visible)) {
            fac <- cos(pi180 * y)      # account for meridional convergence
            dist2 <- ifelse(visible, (fac * (x - argo$longitude))^2 + (y - argo$latitude)^2, 1000)
            i <- which.min(dist2)
            holdLatitude <<- argo$latitude[i]
            holdLongitude <<- argo$longitude[i]
            dist <- sqrt(dist2[i]) * 111 # 1deg lat approx 111km
            lonstring <- ifelse(x < 0, sprintf("%.2fW", abs(argo$longitude[i])), sprintf("%.2fE", x))
            latstring <- ifelse(y < 0, sprintf("%.2fS", abs(argo$latitude[i])), sprintf("%.2fN", y))
            if (length(dist) && dist < 100) {
                highlight <<- TRUE
                rval <- sprintf("%s Float %s cycle %s (type %s from %s) at %s %s, %s",
                    switch(argo$type[i], "core"="Core", "bgc"="BGC", "deep"="Deep"),
                    argo$ID[i],
                    argo$cycle[i],
                    argo$profilerType[i],
                    argo$institution[i],
                    lonstring,
                    latstring,
                    format(argo$time[i], "%Y-%m-%d %H:%M"))
            } else {
                rval <- sprintf("%s %s", lonstring, latstring)
                highlight <<- FALSE
            }
        } else {
            lonstring <- ifelse(x < 0, sprintf("%.2fW", x), sprintf("%.2fE", x))
            latstring <- ifelse(y < 0, sprintf("%.2fS", y), sprintf("%.2fN", y))
            rval <- sprintf("%s %s", lonstring, latstring)
        }
        lastHoverMessage <<- rval
        rval
    }
    })

    shiny::observeEvent(input$brush,
        {
            if (state$polygon == FALSE) {
                state$data <- NULL
                state$polyDone <- FALSE
                argoFloatsDebug(debug,  "observeEvent(input$brush) {\n", style="bold", showTime=FALSE, unindent=1)
                state$xlim <<- c(input$brush$xmin, input$brush$xmax)
                state$ylim <<- c(input$brush$ymin, input$brush$ymax)
                argoFloatsDebug(debug, "set state$xlim=c(",paste(state$xlim,collapse=",),"),
                    " and state$ylim=c(", paste(state$ylim, collapse=","), ")\n")
                argoFloatsDebug(debug,  "} # observeEvent(input$ID)\n", style="bold", showTime=FALSE, unindent=1)
            }
        })

    shiny::observeEvent(input$Ccolour,
        {
            argoFloatsDebug(debug,  "observeEvent(input$Ccolour) {\n", style="bold", showTime=FALSE, unindent=1)
            state$Ccolour <<- input$Ccolour
            argoFloatsDebug(debug, "input$Ccolour= ", input$Ccolour, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$Ccolour)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$Csymbol,
        {
            argoFloatsDebug(debug,  "observeEvent(input$Csymbol) {\n", style="bold", showTime=FALSE, unindent=1)
            state$Csymbol <<- input$Csymbol
            argoFloatsDebug(debug, "input$Csymbol= ", input$Csymbol, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$Csymbol)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$Csize,
        {
            argoFloatsDebug(debug,  "observeEvent(input$Csize) {\n", style="bold", showTime=FALSE, unindent=1)
            state$Csize <<- input$Csize
            argoFloatsDebug(debug, "input$Csize= ", input$Csize, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$Csize)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$Cborder,
        {
            argoFloatsDebug(debug,  "observeEvent(input$Cborder) {\n", style="bold", showTime=FALSE, unindent=1)
            state$Cborder <<- input$Cborder
            argoFloatsDebug(debug, "input$Cborder= ", input$Cborder, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$Cborder)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$CPcolour,
        {
            argoFloatsDebug(debug,  "observeEvent(input$CPcolour) {\n", style="bold", showTime=FALSE, unindent=1)
            state$CPcolour <<- input$CPcolour
            argoFloatsDebug(debug, "input$CPcolour= ", input$CPcolour, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$CPcolour)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$CPwidth,
        {
            argoFloatsDebug(debug,  "observeEvent(input$CPwidth) {\n", style="bold", showTime=FALSE, unindent=1)
            state$CPwidth <<- input$CPwidth
            argoFloatsDebug(debug, "input$CPwidth= ", input$CPwidth, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$CPwidth)\n", style="bold", showTime=FALSE, unindent=1)
        })


 shiny::observeEvent(input$Bcolour,
        {
            argoFloatsDebug(debug,  "observeEvent(input$Bcolour) {\n", style="bold", showTime=FALSE, unindent=1)
            state$Bcolour <<- input$Bcolour
            argoFloatsDebug(debug, "input$Bcolour= ", input$Bcolour, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$Bcolour)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$Bsymbol,
        {
            argoFloatsDebug(debug,  "observeEvent(input$Bsymbol) {\n", style="bold", showTime=FALSE, unindent=1)
            state$Bsymbol <<- input$Bsymbol
            argoFloatsDebug(debug, "input$Bsymbol= ", input$Bsymbol, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$Bsymbol)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$Bsize,
        {
            argoFloatsDebug(debug,  "observeEvent(input$Bsize) {\n", style="bold", showTime=FALSE, unindent=1)
            state$Bsize <<- input$Bsize
            argoFloatsDebug(debug, "input$=Bsize ", input$Bsize, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$Bsize)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$Bborder,
        {
            argoFloatsDebug(debug,  "observeEvent(input$Bborder) {\n", style="bold", showTime=FALSE, unindent=1)
            state$Bborder <<- input$Bborder
            argoFloatsDebug(debug, "input$=Bborder ", input$Bborder, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$Bborder)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$BPcolour,
        {
            argoFloatsDebug(debug,  "observeEvent(input$BPcolour) {\n", style="bold", showTime=FALSE, unindent=1)
            state$BPcolour <<- input$BPcolour
            argoFloatsDebug(debug, "input$=BPcolour ", input$BPcolour, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$BPcolour)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$BPwidth,
        {
            argoFloatsDebug(debug,  "observeEvent(input$BPwidth) {\n", style="bold", showTime=FALSE, unindent=1)
            state$BPwidth <<- input$BPwidth
            argoFloatsDebug(debug, "input$=BPwidth ", input$BPwidth, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$BPwidth)\n", style="bold", showTime=FALSE, unindent=1)
        })

 shiny::observeEvent(input$Dcolour,
        {
            argoFloatsDebug(debug,  "observeEvent(input$Dcolour) {\n", style="bold", showTime=FALSE, unindent=1)
            state$Dcolour <<- input$Dcolour
            argoFloatsDebug(debug, "input$=Dcolour ", input$Dcolour, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$Dcolour)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$Dsymbol,
        {
            argoFloatsDebug(debug,  "observeEvent(input$Dsymbol) {\n", style="bold", showTime=FALSE, unindent=1)
            state$Dsymbol <<- input$Dsymbol
            argoFloatsDebug(debug, "input$=Dsymbol ", input$Dsymbol, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$Dsymbol)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$Dsize,
        {
            argoFloatsDebug(debug,  "observeEvent(input$Dsize) {\n", style="bold", showTime=FALSE, unindent=1)
            state$Dsize <<- input$Dsize
            argoFloatsDebug(debug, "input$=Dsize ", input$Dsize, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$Dsize)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$Dborder,
        {
            argoFloatsDebug(debug,  "observeEvent(input$Dborder) {\n", style="bold", showTime=FALSE, unindent=1)
            state$Dborder <<- input$Dborder
            argoFloatsDebug(debug, "input$=Dborder ", input$Dborder, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$Dborder)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$DPcolour,
        {
            argoFloatsDebug(debug,  "observeEvent(input$DPcolour) {\n", style="bold", showTime=FALSE, unindent=1)
            state$DPcolour <<- input$DPcolour
            argoFloatsDebug(debug, "input$=DPcolour ", input$DPcolour, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$DPcolour)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$DPwidth,
        {
            argoFloatsDebug(debug,  "observeEvent(input$DPwidth) {\n", style="bold", showTime=FALSE, unindent=1)
            state$DPwidth <<- input$DPwidth
            argoFloatsDebug(debug, "input$=DPwidth ", input$DPwidth, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$DPwidth)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$view,
        {
            state$view <<- input$view
        }, ignoreNULL=FALSE)

    shiny::observeEvent(input$action,
        {
            state$action <<- input$action
        }, ignoreNULL=FALSE)

    shiny::observeEvent(input$goE,
        {
            argoFloatsDebug(debug,  "observeEvent(input$goE) {\n", style="bold", showTime=FALSE, unindent=1)
            dx <- diff(state$xlim) # present longitude span
            state$xlim <<- pinlon(state$xlim + dx / 4)
            argoFloatsDebug(debug, "state$xlim ", state$xlim, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$goE)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$goW,
        {
            argoFloatsDebug(debug,  "observeEvent(input$goW) {\n", style="bold", showTime=FALSE, unindent=1)
            dx <- diff(state$xlim) # present longitude span
            state$xlim <<- pinlon(state$xlim - dx / 4)
            argoFloatsDebug(debug, "state$xlim ", state$xlim, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$goW)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$goS,
        {
            argoFloatsDebug(debug,  "observeEvent(input$goS) {\n", style="bold", showTime=FALSE, unindent=1)
            dy <- diff(state$ylim) # present latitude span
            state$ylim <<- pinlat(state$ylim - dy / 4)
            argoFloatsDebug(debug, "state$ylim ", state$ylim, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$goS)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$goN,
        {
            argoFloatsDebug(debug,  "observeEvent(input$goN) {\n", style="bold", showTime=FALSE, unindent=1)
            dy <- diff(state$ylim) # present latitude span
            state$ylim <<- pinlat(state$ylim + dy / 4)
            argoFloatsDebug(debug, "state$ylim ", state$ylim, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$goN)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$zoomIn,
        {
            argoFloatsDebug(debug,  "observeEvent(input$zoomIn) {\n", style="bold", showTime=FALSE, unindent=1)
            state$xlim <<- pinlon(mean(state$xlim)) + c(-0.5, 0.5) / 1.3 * diff(state$xlim)
            state$ylim <<- pinlat(mean(state$ylim)) + c(-0.5, 0.5) / 1.3 * diff(state$ylim)
            argoFloatsDebug(debug, "state$xlim ", state$xlim, " and state$ylim=",state$ylim, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$zoomIn)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$zoomOut,
        {
            argoFloatsDebug(debug,  "observeEvent(input$zoomOut) {\n", style="bold", showTime=FALSE, unindent=1)
            state$xlim <<- pinlon(mean(state$xlim) + c(-0.5, 0.5) * 1.3 * diff(state$xlim))
            state$ylim <<- pinlat(mean(state$ylim) + c(-0.5, 0.5) * 1.3 * diff(state$ylim))
            argoFloatsDebug(debug, "state$xlim ", state$xlim, " and state$ylim=",state$ylim, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$zoomOut)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$code,
        {
            argoFloatsDebug(debug,  "observeEvent(input$code) {\n", style="bold", showTime=FALSE, unindent=1)
            msg <- "library(argoFloats)<br>"
            msg <- paste(msg, "# Download (or use cached) index from one of two international servers.<br>")
            if ("core" %in% state$view && "bgc" %in% state$view && !("deep" %in% state$view)) {
                msg <- paste(msg, "ai <- getIndex()<br>")
                msg <- paste(msg, "bai <- getIndex(filename=\"bgc\")<br>")
                msg <- paste(msg, "mai <- merge(ai,bai)<br>")
                msg <- paste(msg, "# Subset to remove deep profiles.<br>")
                msg <- paste(msg, "index <- subset(mai, deep=FALSE)<br>")
            } else if ("core" %in% state$view && "deep" %in% state$view && !("bgc" %in% state$view)) {
                msg <- paste(msg, "ai <- getIndex()<br>")
                msg <- paste(msg, "bai <- getIndex(filename=\"bgc\")<br>")
                msg <- paste(msg, "# Subset deep profiles.<br>")
                msg <- paste(msg, "deep1 <- subset(ai, deep=TRUE)<br>")
                msg <- paste(msg, "deep2 <- subset(bai, deep=TRUE)<br>")
                msg <- paste(msg, "deep <- merge(deep1,deep2)<br>")
                msg <- paste(msg, "index <- merge(ai,deep)<br>")
            } else if ("bgc" %in% state$view && "deep" %in% state$view && !("core" %in% state$view)) {
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
            } else if ("core" %in% state$view && !("bgc" %in% state$view) && !("deep" %in% state$view)) {
                msg <- paste(msg, "index <- getIndex()<br>")
            } else if ("bgc" %in% state$view && !("core" %in% state$view) && !("deep" %in% state$view)) {
                msg <- paste(msg, "index <- getIndex(filename=\"bgc\")<br>")
            } else if ("deep" %in% state$view && !("core" %in% state$view) && !("bgc" %in% state$view)) {
                msg <- paste(msg, "bai <- getIndex(filename=\"bgc\")<br>")
                msg <- paste(msg, "ai <- getIndex()<br>")
                msg <- paste(msg, "# Subset deep profiles.<br>")
                msg <- paste(msg, "deep1 <- subset(ai, deep=TRUE)<br>")
                msg <- paste(msg, "deep2 <- subset(bai, deep=TRUE)<br>")
                msg <- paste(msg, "index <- merge(deep1,deep2)<br>")
            }
            msg <- paste(msg, "# Subset by time.<br>")
            msg <- paste(msg, "from <- as.POSIXct(\"", format(state$startTime, "%Y-%m-%d", tz="UTC"), "\", tz=\"UTC\")<br>")
            msg <- paste(msg, "to <- as.POSIXct(\"", format(state$endTime, "%Y-%m-%d", tz="UTC"), "\", tz=\"UTC\")<br>", sep="")
            msg <- paste(msg, "subset1 <- subset(index, time=list(from=from, to=to))<br>")
            msg <- paste(msg, "# Subset by space.<br>")
            if (state$polyDone) {
                latp <- paste(round(latpoly,3), collapse=",")
                lonp <- paste(round(lonpoly,3), collapse=",")
                msg <- paste(msg, "subset2 <- subset(subset1, polygon=list(longitude=c(",lonp,"),latitude=c(",latp,")))<br>")
            } else {
                lonRect <- state$xlim
                latRect <- state$ylim
                msg <- paste(msg, sprintf("rect <- list(longitude=c(%.4f,%.4f), latitude=c(%.4f,%.4f))<br>",
                        lonRect[1], lonRect[2], latRect[1], latRect[2]))
                msg <- paste(msg, "subset2 <- subset(subset1, rectangle=rect)<br>")
            }
            if (!is.null(state$focusID)) {
                msg <- paste0(msg, sprintf("subset2 <- subset(subset2, ID=%2s)<br>", state$focusID))
            }
            msg <- paste(msg, "# Plot a map (with different formatting than used here).<br>")
            if ("topo" %in% state$view) {
                if ("path" %in% state$view) {
                    if ("lines" %in% state$action) {
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
                argoFloatsDebug(debug, "state$action= ", state$action, "\n")
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
            argoFloatsDebug(debug, "state$view= ", state$view, "\n")
            argoFloatsDebug(debug, "state$focusID= ", state$focusID, "\n")
            argoFloatsDebug(debug, "state$action= ", state$action, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$code)\n", style="bold", showTime=FALSE, unindent=1)
            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title="R code hints", size="l"))
        })

    shiny::observeEvent(input$ID,
        {
            argoFloatsDebug(debug,  "observeEvent(input$ID) {\n", style="bold", showTime=FALSE, unindent=1)
            if (0 == nchar(input$ID)) {
                argoFloatsDebug(debug, "input$ID is empty, so setting state$focusID to NULL and doing nothing else\n")
                state$focusID <<- NULL
            } else {
                k <- which(argo$ID == input$ID)
                argoFloatsDebug(debug, " have ", length(k), " cycles for input$ID='", input$ID, "'\n", sep="")
                if (length(k) > 0L) {
                    state$focusID <<- input$ID
                    state$xlim <<- pinlon(extendrange(argo$lon[k], f=0.15))
                    state$ylim <<- pinlat(extendrange(argo$lat[k], f=0.15))
                    state$startTime <<- dayStart(min(argo$time[k]))
                    state$endTime <<- dayEnd(max(argo$time[k]))
                    argoFloatsDebug(debug, "set xlim:      ", state$xlim[1], " to ", state$xlim[2], "\n")
                    argoFloatsDebug(debug, "set ylim:      ", state$ylim[1], " to ", state$ylim[2], "\n")
                    argoFloatsDebug(debug, "set startTime: ", format(state$startTime, "%Y-%m-%d %H:%M:%S"), "\n")
                    argoFloatsDebug(debug, "set endTime:   ", format(state$endTime, "%Y-%m-%d %H:%M:%S"), "\n")
                } else {
                    # Float IDs usually (always?) have 7 digits
                    if (nchar(input$ID) > 7L)
                        shiny::showNotification("Float IDs cannot have more than 7 characters", type="error")
                    else if (nchar(input$ID) > 6L)
                        shiny::showNotification(paste0("There is no float with ID ", input$ID, "."), type="error")
                }
            }
            argoFloatsDebug(debug,  "} # observeEvent(input$ID)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$dblclick,
        {
            argoFloatsDebug(debug,  "observeEvent(input$dblclick) {\n", style="bold", showTime=FALSE, unindent=1)
            x <- input$dblclick$x
            y <- input$dblclick$y
            state$data <- NULL
            state$polyDone <- FALSE
            state$polygon <- FALSE
            shinyBS::updateButton(session, "polygon", style="default")
            fac2 <- 1.0/cos(pi180*y)^2 # for deltaLon^2 compared with deltaLat^2
            if (!is.null(state$focusID)) {
                argoFloatsDebug(debug, "state$focusID is NULL\n")
                keep <- argo$ID == state$focusID
            } else {
                # Restrict search to the present time window
                argoFloatsDebug(debug, "state$focusID='", state$focusID, "'\n", sep="")
                keep <- state$startTime <= argo$time & argo$time <= state$endTime
            }
            i <- which.min(ifelse(keep, fac2*(x-argo$longitude)^2+(y-argo$latitude)^2, 1000))
            if (argo$type[i] %in% state$view) {
                state$focusID <<- argo$ID[i]
                k <- which(argo$ID == argo$ID[i])
                argoFloatsDebug(debug, "have ", length(k), " cycles for inferred ID='", argo$ID[i], "'\n", sep="")
                state$xlim <<- pinlon(extendrange(argo$lon[k], f=0.15))
                state$ylim <<- pinlat(extendrange(argo$lat[k], f=0.15))
                state$startTime <<- dayStart(min(argo$time[k]))
                state$endTime <<- dayEnd(max(argo$time[k]))
                argoFloatsDebug(debug, "set xlim:      ", state$xlim[1], " to ", state$xlim[2], "\n")
                argoFloatsDebug(debug, "set ylim:      ", state$ylim[1], " to ", state$ylim[2], "\n")
                argoFloatsDebug(debug, "set startTime: ", format(state$startTime, "%Y-%m-%d %H:%M:%S"), "\n")
                argoFloatsDebug(debug, "set endTime:   ", format(state$endTime, "%Y-%m-%d %H:%M:%S"), "\n")
            } else {
                argoFloatsDebug(debug, "float ID=\"", state$focusID, "\" is NOT in view\n", sep="")
            }
            argoFloatsDebug(debug,  "} # observeEvent(input$dblclick)\n", style="bold", showTime=FALSE, unindent=1)
        })
    var1 <- list()
    var2 <- list()
    data <- cbind(var1, var2)
    shiny::observeEvent(input$click,
        {
            argoFloatsDebug(debug,  "observeEvent(input$click) {\n", style="bold", showTime=FALSE, unindent=1)
            argoFloatsDebug(debug, "state$polygon= ", state$polygon, "and state$polyDone = ", state$polyDone, "\n")
            if (input$polygon) {
                argoFloatsDebug(debug, "state$polygon= ", state$polygon, "and state$polyDone = ", state$polyDone, "\n")
                state$click$x <<- input$click$x
                state$click$y <<- input$click$y
                state$data <- rbind(state$data, cbind(input$click$x, input$click$y))
                lonpoly <<- unlist(state$data[,1])
                latpoly <<- unlist(state$data[,2])
            }
            argoFloatsDebug(debug,  "} # observeEvent(input$click)\n", style="bold", showTime=FALSE, unindent=1)
        })
    shiny::observeEvent(input$polygon,
        {
            argoFloatsDebug(debug,  "observeEvent(input$polygon) {\n", style="bold", showTime=FALSE, unindent=1)
            state$polygon <<- input$polygon
            shinyBS::updateButton(session, "polygon", style="danger")
            if (state$polygon > 1) {
                state$data <- NULL
            }
            argoFloatsDebug(debug, "state$polygon= ", state$polygon, "and state$polyDone = ", state$polyDone, "\n")
            argoFloatsDebug(debug,  "} # observeEvent(input$click)\n", style="bold", showTime=FALSE, unindent=1)
        })
    shiny::observeEvent(input$start,
        {
            argoFloatsDebug(debug,  "observeEvent(input$start) {\n", style="bold", showTime=FALSE, unindent=1)
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
                    argoFloatsDebug(debug, "user selected start time ", format(t, "%Y-%m-%d %H:%M:%S %z"), "\n")
                }
            }
            argoFloatsDebug(debug,  "} # observeEvent(input$start)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$end,
        {
            argoFloatsDebug(debug,  "observeEvent(input$end) {\n", style="bold", showTime=FALSE, unindent=1)
            if (0 == nchar(input$end)) {
                state$endTime <<- max(argo$time, na.rm = TRUE)
            } else {
                t <- try(as.POSIXct(format(input$end, "%Y-%m-%d 00:00:00"), tz="UTC"), silent=TRUE)
                if (inherits(t, "try-error")) {
                    shiny::showNotification(paste0( "End time \"", input$end, "\" is not in yyyy-mm-dd format, or is otherwise invalid."), type = "error")
                } else {
                    state$endTime <<- t
                    argoFloatsDebug(debug, "user selected end time ", format(t, "%Y-%m-%d %H:%M:%S %z"), "\n")
                }
            }
            argoFloatsDebug(debug,  "} # observeEvent(input$end)\n", style="bold", showTime=FALSE, unindent=1)
        })

    shiny::observeEvent(input$help,
        {
            msg <- shiny::HTML(overallHelp)
            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title="Using mapApp()", size="l"))
        })


    shiny::observeEvent(input$undo,
        {
            if (sizeState() > 2L) {
                popState()
                previousState <- topState()
                colourpicker::updateColourInput(session, inputId="Ccolour", value=previousState$Ccolour)
                colourpicker::updateColourInput(session, inputId="Cborder", value=previousState$Cborder)
                colourpicker::updateColourInput(session, inputId="CPcolour", value=previousState$CPcolour)
                colourpicker::updateColourInput(session, inputId="Bcolour", value=previousState$Bcolour)
                colourpicker::updateColourInput(session, inputId="Bborder", value=previousState$Bborder)
                colourpicker::updateColourInput(session, inputId="BPcolour", value=previousState$BPcolour)
                colourpicker::updateColourInput(session, inputId="Dcolour", value=previousState$Dcolour)
                colourpicker::updateColourInput(session, inputId="Dborder", value=previousState$Dborder)
                colourpicker::updateColourInput(session, inputId="DPcolour", value=previousState$DPcolour)
                shiny::updateNumericInput(session, inputId="Csymbol", value=previousState$Csymbol)
                shiny::updateSliderInput(session, inputId="Csize", value=previousState$Csize)
                shiny::updateSliderInput(session, inputId="CPwidth", value=previousState$CPwidth)
                shiny::updateNumericInput(session, inputId="Bsymbol", value=previousState$Bsymbol)
                shiny::updateSliderInput(session, inputId="Bsize", value=previousState$Bsize)
                shiny::updateSliderInput(session, inputId="BPwidth", value=previousState$BPwidth)
                shiny::updateNumericInput(session, inputId="Dsymbol", value=previousState$Dsymbol)
                shiny::updateSliderInput(session, inputId="Dsize", value=previousState$Dsize)
                shiny::updateSliderInput(session, inputId="DPwidth", value=previousState$DPwidth)
                for (name in names(previousState))
                    state[[name]] <- previousState[[name]]
            }
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
            argoFloatsDebug(debug,  "observeEvent(input$keypressTrigger) {\n", style="bold", showTime=FALSE, unindent=1)
            key <- intToUtf8(input$keypress)
            #message(input$keypress)
            #message(key)
            if (key == "d") { # toggle debug
                debug <<- !debug
                message("switched debug to ", debug)
            #} else if (key == "n") { # go north
            #    dy <- diff(state$ylim)
            #    state$ylim <<- pinlat(state$ylim + dy / 4)
            #} else if (key == "s") { # go south
            #    dy <- diff(state$ylim)
            #    state$ylim <<- pinlat(state$ylim - dy / 4)
            #} else if (key == "e") { # go east
            #    dx <- diff(state$xlim)
            #    state$xlim <<- pinlon(state$xlim + dx / 4)
            #} else if (key == "w") { # go west
            #    dx <- diff(state$xlim)
            #    state$xlim <<- pinlon(state$xlim - dx / 4)
            #} else if (key == "f") { # forward in time
            #    interval <- as.numeric(state$endTime) - as.numeric(state$startTime)
            #    state$startTime <<- state$startTime + interval
            #    state$endTime <<- state$endTime + interval
            #    shiny::updateTextInput(session, "start", value=format(state$startTime, "%Y-%m-%d"))
            #    shiny::updateTextInput(session, "end", value=format(state$endTime, "%Y-%m-%d"))
            #} else if (key == "b") { # backward in time
            #    interval <- as.numeric(state$endTime) - as.numeric(state$startTime)
            #    state$startTime <<- state$startTime - interval
            #    state$endTime <<- state$endTime - interval
            #    shiny::updateTextInput(session, "start", value=format(state$startTime, "%Y-%m-%d"))
            #    shiny::updateTextInput(session, "end", value=format(state$endTime, "%Y-%m-%d"))
            #} else if (key == "i") { # zoom in
            #    state$xlim <<- pinlon(mean(state$xlim)) + c(-0.5, 0.5) / 1.3 * diff(state$xlim)
            #    state$ylim <<- pinlat(mean(state$ylim)) + c(-0.5, 0.5) / 1.3 * diff(state$ylim)
            #} else if (key == "o") { # zoom out
            #    state$xlim <<- pinlon(mean(state$xlim) + c(-0.5, 0.5) * 1.3 * diff(state$xlim))
            #    state$ylim <<- pinlat(mean(state$ylim) + c(-0.5, 0.5) * 1.3 * diff(state$ylim))
            } else if (key == "h") { # paste hover message
                argoFloatsDebug(debug, "Key h pressed, so setting state$hoverIsPasted to", !state$hoverIsPasted, "\n")
                state$hoverIsPasted <<- !state$hoverIsPasted
                pushState(isolate(reactiveValuesToList(state)))
            } else if (key == "u") {
                if (sizeState() > 2L) {
                    popState()
                    previousState <- topState()
                    colourpicker::updateColourInput(session, inputId="Ccolour", value=previousState$Ccolour)
                    colourpicker::updateColourInput(session, inputId="Cborder", value=previousState$Cborder)
                    colourpicker::updateColourInput(session, inputId="CPcolour", value=previousState$CPcolour)
                    colourpicker::updateColourInput(session, inputId="Bcolour", value=previousState$Bcolour)
                    colourpicker::updateColourInput(session, inputId="Bborder", value=previousState$Bborder)
                    colourpicker::updateColourInput(session, inputId="BPcolour", value=previousState$BPcolour)
                    colourpicker::updateColourInput(session, inputId="Dcolour", value=previousState$Dcolour)
                    colourpicker::updateColourInput(session, inputId="Dborder", value=previousState$Dborder)
                    colourpicker::updateColourInput(session, inputId="DPcolour", value=previousState$DPcolour)
                    shiny::updateNumericInput(session, inputId="Csymbol", value=previousState$Csymbol)
                    shiny::updateSliderInput(session, inputId="Csize", value=previousState$Csize)
                    shiny::updateSliderInput(session, inputId="CPwidth", value=previousState$CPwidth)
                    shiny::updateNumericInput(session, inputId="Bsymbol", value=previousState$Bsymbol)
                    shiny::updateSliderInput(session, inputId="Bsize", value=previousState$Bsize)
                    shiny::updateSliderInput(session, inputId="BPwidth", value=previousState$BPwidth)
                    shiny::updateNumericInput(session, inputId="Dsymbol", value=previousState$Dsymbol)
                    shiny::updateSliderInput(session, inputId="Dsize", value=previousState$Dsize)
                    shiny::updateSliderInput(session, inputId="DPwidth", value=previousState$DPwidth)
                    for (name in names(previousState))
                        state[[name]] <- previousState[[name]]
                }

            } else if (key == "r") { # reset to start
                argoFloatsDebug(debug, "Key r pressed \n")
                shinyBS::updateButton(session, "polygon", style="default")
                state$xlim <<- c(-180, 180)
                state$ylim <<- c(-90, 90)
                state$startTime <<- startTime
                state$endTime <<- endTime
                state$focusID <<- NULL
                state$hoverIsPasted <<- FALSE
                state$polyDone <- FALSE
                state$data <<- NULL
                shiny::updateCheckboxGroupInput(session, "show", selected=character(0))
                shiny::updateCheckboxGroupInput(session, "view", selected=c("core", "deep", "bgc"))
                shiny::updateSelectInput(session, "action", selected=NULL)
                shiny::updateDateInput(session, inputId="start", label="Start", value=startTime)
                shiny::updateDateInput(session, inputId="end", label="End", value=endTime)
                state$view <<- c("core", "deep", "bgc")
                state$action <<- NULL
                # core
                colourpicker::updateColourInput(session, inputId="Ccolour", value=colDefaults$core)
                colourpicker::updateColourInput(session, inputId="Cborder", value="black")
                colourpicker::updateColourInput(session, inputId="CPcolour", value=colDefaults$core)
                shiny::updateNumericInput(session, inputId="Csymbol", value=21)
                shiny::updateSliderInput(session, inputId="Csize", value=0.9)
                shiny::updateSliderInput(session, inputId="CPwidth", value=1.4)
                # bgc
                colourpicker::updateColourInput(session, inputId="Bcolour", value=colDefaults$bgc)
                colourpicker::updateColourInput(session, inputId="Bborder", value="black")
                colourpicker::updateColourInput(session, inputId="BPcolour", value=colDefaults$bgc)
                shiny::updateNumericInput(session, inputId="Bsymbol", value=21)
                shiny::updateSliderInput(session, inputId="Bsize", value=0.9)
                shiny::updateSliderInput(session, inputId="BPwidth", value=1.4)
                # deep
                colourpicker::updateColourInput(session, inputId="Dcolour", value=colDefaults$deep)
                colourpicker::updateColourInput(session, inputId="Dborder", value="black")
                colourpicker::updateColourInput(session, inputId="DPcolour", value=colDefaults$deep)
                shiny::updateNumericInput(session, inputId="Dsymbol", value=21)
                shiny::updateSliderInput(session, inputId="Dsize", value=0.9)
                shiny::updateSliderInput(session, inputId="DPwidth", value=1.4)
            } else if (key == "0") { # Unzoom an area and keep same time scale
                argoFloatsDebug(debug, "Key 0 pressed \n")
                state$xlim <<- c(-180, 180)
                state$ylim <<- c(-90, 90)
                shiny::updateCheckboxGroupInput(session, "show", selected=character(0))
            } else if (key == "?") { # show help on keystrokes
                argoFloatsDebug(debug, "Key ? pressed \n")
                shiny::showModal(shiny::modalDialog(title="Key-stroke commands",
                        shiny::HTML(keyPressHelp), easyClose=TRUE))
            } else if (key == "q") {
                argoFloatsDebug(debug, "Key q pressed \n")
                if (state$polygon) {
                    if (length(lonpoly) < 3) {
                        shiny::showNotification("Must choose at least 3 points for polygon.", type="message", duration=5)
                    } else {
                        state$polyDone <- TRUE
                        state$polygon <- FALSE
                        POLY <- subset(m, polygon=list(longitude=lonpoly, latitude=latpoly), silent=TRUE)
                        polykeep <<- (argo[["file"]] %in% POLY[["file"]])
                        state$xlim <<- c(min(lonpoly), max(lonpoly))
                        state$ylim <<- c(min(latpoly), max(latpoly))
                        shinyBS::updateButton(session, "polygon", style="default")
                    }
                }
            }
            argoFloatsDebug(debug,  "} # observeEvent(input$keypressTrigger)\n", style="bold", showTime=FALSE, unindent=1)
        })                                  # keypressTrigger

    output$plotMap <- shiny::renderPlot({
        argoFloatsDebug(debug, "plotMap() {\n", style="bold", unindent=1)
        argoFloatsDebug(debug, "plotCounter=", plotCounter, "\n", sep="")
        argoFloatsDebug(debug, "lastHoverMessage=", lastHoverMessage, "\n", sep="")
        argoFloatsDebug(debug, "state$hoverIsPasted=", state$hoverIsPasted, "\n", sep="")
        plotCounter <<- plotCounter + 1L
        #> message("in output$plotMap with state$begin=", state$begin)
        if (state$begin)
            state$view <<- viewDefaults
        state$begin <<- FALSE
        validate(need(state$startTime < state$endTime,
                "The Start time must precede the End time."))
        omar <- par("mar")
        omgp <- par("mgp")
        par(mar=c(2.0, 2.0, 1.0, 1.0), mgp=c(2, 0.75, 0))
        on.exit(par(mar=omar, mgp=omgp))
        topo <- if ("hires" %in% state$view) topoWorldFine else topoWorld
        argoFloatsDebug(debug, "calling plot() to set up scales\n")
        if (plotCounter < 4) {
            argoFloatsDebug(debug, "} # plotMap() early return since plotCounter=", plotCounter, "\n", sep="", style="bold", unindent=1)
            return()
        }
        plot(state$xlim, state$ylim, xlab="", ylab="", axes=FALSE, type="n", asp=1 / cos(pi180 * mean(state$ylim)))
        if (debug > 0) {
            mtext(plotCounter, adj=1, col=2)
        }
        if ("topo" %in% state$view) {
            argoFloatsDebug(debug, "drawing topo as an image\n")
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
        argoFloatsDebug(debug, "drawing coastline as a polygon\n")
        polygon(coastline[["longitude"]], coastline[["latitude"]], col=colLand)
        rect(usr[1], usr[3], usr[2], usr[4], lwd = 1)
        # For focusID mode, we do not trim by time or space
        keep <- if (!is.null(state$focusID)) {
            argo$ID == state$focusID
        }  else {
            rep(TRUE, length(argo$ID))
        }
        argoFloatsDebug(debug, "subset from", format(state$startTime, "%Y-%m-%d %H:%M:%S %z"), "to", format(state$endTime, "%Y-%m-%d %H:%M:%S %z"), "\n")
        keep <- keep & (state$startTime <= argo$time & argo$time <= state$endTime)
        keep <- keep & (state$xlim[1] <= argo$longitude & argo$longitude <= state$xlim[2])
        keep <- keep & (state$ylim[1] <= argo$latitude & argo$latitude <= state$ylim[2])
        if (state$polyDone && is.null(state$focusID)) {
            keep <- keep & polykeep
        }
        argoFloatsDebug(debug, "subsetting for time and space leaves", sum(keep), "profiles, in categories:\n")
        cex <- 0.9
        counts <- ""
        if (sum(c("core", "deep", "bgc") %in% state$view) > 0) {
            # Draw points, optionally connecting paths (and indicating start points)
            visible <<- rep(FALSE, length(argo$lon))
            for (view in c("core", "deep", "bgc")) {
                if (view %in% state$view) {
                    k <- keep & argo$type == view
                    visible <<- visible | k
                    lonlat <- argo[k,]
                    counts <- paste0(counts, view, ":", sum(k), " ")
                    colSettings <- list(core=if (state$Ccolour == "default") colDefaults$core else state$Ccolour,
                        bgc=if (state$Bcolour == "default") colDefaults$bgc else state$Bcolour,
                        deep=if (state$Dcolour == "default") colDefaults$deep else state$Dcolour)
                    symbSettings <- list(core=state$Csymbol, bgc=state$Bsymbol, deep=state$Dsymbol)
                    borderSettings <- list(core=state$Cborder, bgc=state$Bborder, deep=state$Dborder)
                    sizeSettings <- list(core=state$Csize, bgc=state$Bsize, deep=state$Dsize)
                    if ("lines" %in% state$action && !"path" %in% state$view) {
                        state$action <- NULL
                    }
                    if (!"lines" %in% state$action) {
                        if (symbSettings[[view]] == 21) {
                            points(lonlat$lon, lonlat$lat, pch=symbSettings[[view]], cex=sizeSettings[[view]], bg=colSettings[[view]], col=borderSettings[[view]], lwd=0.5)
                        } else {
                            points(lonlat$lon, lonlat$lat, pch=symbSettings[[view]], cex=sizeSettings[[view]], col=colSettings[[view]], bg=colSettings[[view]], lwd=0.5)
                        }
                    }
                    if (state$hoverIsPasted && highlight == TRUE) {
                        points(holdLongitude, holdLatitude, pch=21, col="red", bg="red")
                    }
                    if (!(state$polygon %in% FALSE)) {
                        points(unlist(state$data[,1]), unlist(state$data[,2]), pch=20, col="red", type="o", lwd=2)
                    }
                    if (state$polyDone && state$polygon == FALSE) {
                        polygon(unlist(state$data[,1]), unlist(state$data[,2]), border="gray", col=NA, lwd=2)
                    }
                    if ("path" %in% state$view) {
                        for (ID in unique(lonlat$ID)) {
                            LONLAT <- lonlat[lonlat$ID==ID,] # will be redefined in this loop
                            # Sort by time instead of relying on the order in the repository
                            o <- order(LONLAT$time)
                            LONLAT <- LONLAT[o, ]
                            no <- length(o)
                            startCycle <- list(longitude=head(LONLAT$lon, 1),
                                latitude=head(LONLAT$lat, 1),
                                cycle=head(LONLAT$cycle, 1))
                            endCycle <- list(longitude=tail(LONLAT$lon, 1),
                                latitude=tail(LONLAT$lat, 1),
                                cycle=tail(LONLAT$cycle, 1))
                            if (no > 1) {
                                pathColour <- list(core=if (state$CPcolour == "default") colDefaults$core else state$CPcolour,
                                    bgc=if (state$BPcolour == "default") colDefaults$bgc else state$BPcolour,
                                    deep=if (state$DPcolour == "default") colDefaults$deep else state$DPcolour)
                                pathWidth <- list(core=state$CPwidth, bgc=state$BPwidth, deep=state$DPwidth)
                                #message(pathColour[[view]], " is the path color")
                                # Chop data at the dateline
                                # https://github.com/ArgoCanada/argoFloats/issues/503
                                LONLAT <- sf::st_wrap_dateline(
                                    sf::st_sfc(
                                        sf::st_linestring(cbind(LONLAT$lon, LONLAT$lat)),
                                        crs="OGC:CRS84"))[[1]]
                                # message("class(lonlatSegments): ", paste(class(lonlatSegments), collapse=" "))
                                # Examination with the above indicates two choices: LINESTRING and MULTILINESTRING
                                if (inherits(LONLAT, "LINESTRING")) {
                                    lines(LONLAT[,1], LONLAT[,2],
                                        col=pathColour[[view]], lwd=pathWidth[[view]])
                                    if ("start" %in% state$action)
                                        points(startCycle$longitude, startCycle$latitude, pch=2, cex=1, lwd=1.4)
                                    if ("end" %in% state$action)
                                        points(endCycle$longitude, endCycle$latitude, pch=23, cex=1, lwd=1.4)
                                } else if (inherits(LONLAT, "MULTILINESTRING")) {
                                    for (seg in LONLAT) {
                                        lines(seg[,1], seg[,2], col=pathColour[[view]], lwd=1.4)
                                        if ("start" %in% state$action)
                                            points(startCycle$longitude, startCycle$latitude, pch=2, cex=1, lwd=1.4)
                                        if ("end" %in% state$action)
                                            points(endCycle$longitude, endCycle$latitude, pch=23, cex=1, lwd=1.4)
                                    }
                                }
                            }
                        }
                    }
                }
            }
            # Draw the inspection rectangle as a thick gray line, but only if zoomed
            if (-180 < state$xlim[1] || state$xlim[2] < 180 || -90 < state$ylim[1] || state$ylim[2] < 90)
                if (!(state$polyDone)) {
                    rect(state$xlim[1], state$ylim[1], state$xlim[2], state$ylim[2], border="darkgray", lwd=4)
                }
            # Write a margin comment
            argoFloatsDebug(debug, counts, "\n")
            if (!is.null(state$focusID)) {
                argoFloatsDebug(debug, "single-ID with", sum(visible), "profiles\n")
                mtext(sprintf("Float %s: %s to %s",
                        state$focusID,
                        format(state$startTime, "%Y-%m-%d", tz="UTC"),
                        format(state$endTime, "%Y-%m-%d", tz="UTC")),
                    side=3, cex=0.8 * par("cex"), line=0)
            } else {
                argoFloatsDebug(debug, "multi-ID with", sum(visible), "profiles\n")
                mtext(sprintf("%s to %s: %d Argo profiles",
                        format(state$startTime, "%Y-%m-%d", tz="UTC"),
                        format(state$endTime, "%Y-%m-%d", tz="UTC"),
                        sum(visible)),
                    side=3, line=0, cex=0.8 * par("cex"))
            }
        }                          # if (sum(c("core", "deep", "bgc") %in% state$view) > 0)
        # Add depth contours, if requested.
        if ("contour" %in% state$view)
            contour(topo[["longitude"]], topo[["latitude"]], topo[["z"]],
                levels=-1000*(1:10), drawlabels=FALSE, add=TRUE)
        # Add a scalebar, if we are zoomed in sufficiently. This whites-out below, so
        # we must do this as the last step of drawing.
        if (diff(range(state$ylim)) < 90 && sum(visible)) {
        argoFloatsDebug(debug, "drawing a scale bar because range(state$ylim)= ", range(state$ylim), "\n")
            oce::mapScalebar(x="topright", cex=0.8) }
        pushState(isolate(reactiveValuesToList(state)), debug=debug-1L)
        argoFloatsDebug(debug, "} # plotMap()\n", style="bold", unindent=1)
    }, execOnResize=TRUE, pointsize=18) # plotMap
}                                      # serverMapApp

shiny::shinyApp(ui=uiMapApp, server=serverMapApp)
