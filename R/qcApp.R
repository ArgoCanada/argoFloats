library(shiny)

uiQCApp <- fluidPage(
                shiny::fluidRow(shiny::uiOutput(outputId="UIwidget")),
                shiny::fluidRow(shiny::uiOutput(outputId="UIview")),
                shiny::fluidRow(shiny::uiOutput(outputId="UIsingleQC")),
                shiny::fluidRow(shiny::textOutput("showQCTests"),
                                shiny::uiOutput(outputId="UIcycleWidget")),
                shiny::fluidRow(shiny::plotOutput(outputId="plotMap", dblclick=shiny::dblclickOpts("dblclick"))),
                shiny::fluidRow(shiny::plotOutput(outputId="qcPlot"))

)

QCAppserver <- shinyServer(function(input,output){

                               if (!exists("argos")) {
                                   notificationId <- shiny::showNotification("You must first save an rda file from mapApp() before using qcApp", type="message", duration=NULL)
                               }

                          shiny::observeEvent(input$applyQC,
                                              {
                                                  if (input$applyQC == TRUE)
                                                  clean <<- applyQC(argos)
                                              })


                          output$UIwidget <- shiny::renderUI({
                                  shiny::fluidRow(shiny::column(2,shiny::span(shiny::HTML(paste("<b style=\"color:black; margin-left:1em;\">  ","qcApp 0.1","</b>")))),
                                                  shiny::column(2, shiny::actionButton("read", "Read")),
                                                  shiny::column(2, shiny::actionButton("help", "Help")),
                                                  shiny::column(2, shiny::actionButton("code", "Code")),
                                                  shiny::column(2, shiny::actionButton("undo", "Undo")))
                          })


                          output$UIview <- shiny::renderUI({
                              ## NOTE: The type and colorBy are the same as oceanglider

                                  shiny::fluidRow(
                                                  shiny::column(2, shiny::selectInput("focus", "Focus",choices=c("All", "Single"), selected=c("All"))),
                                                  shiny::column(2, shiny::selectInput("type", "Plot Type",choices=c("TS <T>"="TS",
                          "density(p)"="density profile",
                          "S(p)"="salinity profile",
                          "spiciness(p)"="spiciness profile",
                          "T(p)"="temperature profile",
                          "hist(p)"="pressure histogram",
                          "hist(S)"="salinity histogram",
                          "hist(T)"="temperature histogram"),
                selected="TS")),

                                  shiny::column(2, shiny::selectInput(inputId="colorBy",
                label="Color by",
                choices=c("distance"="distance",
                          "latitude"="latitude",
                          "longitude"="longitude",
                          "p"="pressure",
                          "T"="temperature",
                          "S"="salinity",
                          "density"="sigma0",
                          "N2"="N2",
                          "instability"="instability",
                          "oxygen",
                          "navState"="navState",
                          "tSincePowerOn"="tSincePowerOn",
                          "spiciness"="spiciness0",
                          "(none)"="(none)"),
                selected="(none)")),
                                                  shiny::column(2, shiny::checkboxInput("applyQC", "applyQC", value=FALSE))
                                                  )}

                          )

                          output$UIsingleQC <- shiny::renderUI({
                                 if (input$focus %in% c("Single")) {
                                  shiny::fluidRow(
                                                  shiny::column(2, shiny::textInput("ID", "Float ID", value="", width="11em")),
                                                  shiny::column(2, shiny::textInput("cycle", "Cycle", value="", width="11em")),
                                                  shiny::column(2, shiny::checkboxGroupInput("qc",
                                                                                             label="Single float QC",
                                                                                             choiceNames=list(shiny::tags$span("showQCTests", style="color:black;"),
                                                                                                              shiny::tags$span("Diagnostics", style="color:black;")),
                                                                                             choiceValues=list("showQCTests", "diagnostics"),
                                                                                             inline=TRUE)))
                          } }
                          )

                          ##FIXME: I need to fix placement of this widget
                          output$UIcycleWidget <- shiny::renderUI({
                                                  if (0 != nchar(input$cycle)) {
                                                  shiny::column(2, shiny::checkboxInput("cyclePlot", "Focus on Single cycle", value=FALSE))
                                                  }
                          })

                          shiny::observeEvent(input$ID,
                                              {
                                                  if (0 == nchar(input$ID)) {
                                                      msg <- shiny::HTML("FIXME:: This still needs to me coded in (float ID)")
                                                  } else if (0 != nchar(input$ID)) {
                                                      iid <<- subset(index3, ID=input$ID)
                                                      aid <<- readProfiles(getProfiles(iid))
                                                      cid <<- applyQC(aid)

                                              }})

                          shiny::observeEvent(input$cycle,
                                              {
                                                  if (0 == nchar(input$cycle)) {
                                                      msg <- shiny::HTML("FIXME:: This still needs to me coded in (float ID)")
                                                  } else if (0 != nchar(input$cycle)) {
                                                      ic <- subset(iid, cycle=input$cycle)
                                                      ac <<- readProfiles(getProfiles(ic))
                                                      cc <<- applyQC(ac)
                                                  }})

                          shiny::observeEvent(input$help,
                        {
                            msg <- shiny::HTML("This GUI is used to analyze QC of Argo profiles.<br><br>The Read button is first used to read profiles from an rda previously saved in mapApp(). The Code button brings up a window showing R code that isolates to the view shown and demonstrates some further operations.<br><br>The Plot type determines which plot should be displayed, with the option to color code by a variety of parameters. If the user changes the focus to Single, the user has the option to input the float ID and cycle of interest, with the option to perform a variety of QC. ")
                            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title="Using this application", size="l"))
                        })
                          shiny::observeEvent(input$read,
                        {
                            load(rda)
                            argos <<- readProfiles(getProfiles(index3))

                        })

                          shiny::observeEvent(input$code,
                        {
                            msg <- shiny::HTML("FIXME:: This still needs to me coded in")
                            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title="Using this application", size="l"))
                        })

                          shiny::observeEvent(input$undo,
                        {
                            msg <- shiny::HTML("FIXME:: This still needs to me coded in")
                            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title="Using this application", size="l"))
                        })



shiny::observeEvent(input$qc,
                    {
                        if (input$qc %in%"showQCTests"){
                            output$showQCTests <- renderPrint({
                                message(showQCTests(argos[[1]]))
                            })
                        }
                    })

output$qcPlot <- shiny::renderPlot(

                                   {
                                       if (input$qc %in% "diagnostics") {
                                           i <- subset(index3, ID=input$ID)
                                           argoD <- readProfiles(getProfiles(i))
                                           plot(argoD, which="QC", QCControl=list(parameter="temperature"))
                                   }})

#' @importFrom stats sd
#' @importFrom graphics hist
output$plotMap <- shiny::renderPlot({

        colHistMean <- "forestgreen"
        colHist3SD <- "red"

        ## FIXME: I am working on adding cycle subset

        if (input$type =="TS" && input$applyQC == FALSE && input$focus == "All") {
            plot(argos, which="TS")
        } else if (input$type =="TS" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 == nchar(input$cycle)) {
            plot(aid, which="TS")
        } else if (input$type =="TS" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == FALSE ) {
            plot(aid, which="TS", col="lightgray")
            points(unlist(ac[["SA"]]), unlist(ac[["CT"]]), col="black", pch=20)
        } else if (input$type =="TS" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == TRUE ) {
            plot(ac, which="TS")
        } else if (input$type =="TS" && input$applyQC == TRUE && input$focus =="All") {
            plot(clean, which="TS")
        } else if (input$type == "TS" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 == nchar(input$cycle)) {
            plot(cid, which="TS")
        } else if (input$type =="TS" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == FALSE ) {
            plot(cid, which="TS")
            points(unlist(cc[["SA"]]), unlist(cc[["CT"]]), col="black", pch=20)
        } else if (input$type =="TS" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == TRUE ) {
            plot(cc, which="TS")
        }

                        if (input$type =="density profile" && input$applyQC == FALSE && input$focus == "All") {
                            plot(argos, which="profile", profileControl=list(parameter="sigma0"))
                        } else if (input$type =="density profile" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 == nchar(input$cycle)) {
                            plot(aid, which="profile", profileControl=list(parameter="sigma0"))
                            ## NEW HERE
                        } else if (input$type =="density profile" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == FALSE ) {
                            plot(aid, which="profile", profileControl=list(parameter="sigma0"), col="lightgray")
                            points(unlist(ac[["sigma0"]]), unlist(ac[["pressure"]]), col="black", pch=20)
                        } else if (input$type =="density profile" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == TRUE ) {
                            plot(ac, which="profile", profileControl=list(parameter="sigma0"))
                            ## END NEW
                        } else if (input$type =="density profile" && input$applyQC == TRUE && input$focus == "All") {
                            plot(clean, which="profile", profileControl=list(parameter="sigma0"))
                        } else if (input$type == "density profile" && input$applyQC == TRUE &&  0 != nchar(input$ID) && 0 == nchar(input$cycle)) {
                            plot(cid, which="profile", profileControl=list(parameter="sigma0"))
                        } else if (input$type =="density profile" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == FALSE ) {
                            plot(cid, which="profile", profileControl=list(parameter="sigma0"), col="lightgray")
                            points(unlist(cc[["sigma0"]]), unlist(cc[["pressure"]]), col="black", pch=20)
                        } else if (input$type =="density profile" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == TRUE ) {
                            plot(cc, which="profile", profileControl=list(parameter="sigma0"))
                        }

                        if (input$type =="salinity profile" && input$applyQC == FALSE && input$focus == "All") {
                            plot(argos, which="profile", profileControl=list(parameter="SA"))
                        } else if (input$type =="salinity profile" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 == nchar(input$cycle)) {
                            plot(aid, which="profile", profileControl=list(parameter="SA"))
                        } else if (input$type =="salinity profile" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == FALSE) {
                            plot(aid, which="profile", profileControl=list(parameter="SA"), col="lightgray")
                            points(unlist(ac[["SA"]]), unlist(ac[["pressure"]]), col="black", pch=20)
                        } else if (input$type =="salinity profile" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == TRUE) {
                            plot(ac, which="profile", profileControl=list(parameter="SA"))
                        } else if (input$type =="salinity profile" && input$applyQC == TRUE && input$focus == "All") {
                            plot(clean, which="profile", profileControl=list(parameter="SA"))
                        } else if (input$type == "salinity profile" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 == nchar(input$cycle)) {
                            plot(cid, which="profile", profileControl=list(parameter="SA"))
                        } else if (input$type =="salinity profile" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == FALSE ) {
                            plot(cid, which="profile", profileControl=list(parameter="SA"), col="lightgray")
                            points(unlist(cc[["SA"]]), unlist(cc[["pressure"]]), col="black", pch=20)
                        } else if (input$type =="salinity profile" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == TRUE ) {
                            plot(cc, which="profile", profileControl=list(parameter="SA"))
                        }

                        if (input$type =="spiciness profile" && input$applyQC == FALSE && input$focus == "All") {
                            plot(argos, which="profile", profileControl=list(parameter="spice"))
                        } else if (input$type =="spiciness profile" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 == nchar(input$cycle)) {
                            plot(aid, which="profile", profileControl=list(parameter="spice"))
                        } else if (input$type =="spiciness profile" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == FALSE ) {
                            plot(aid, which="profile", profileControl=list(parameter="spice"), col="lightgray")
                            points(unlist(ac[["spice"]]), unlist(ac[["pressure"]]), col="black", pch=20)
                        } else if (input$type =="spiciness profile" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == TRUE ) {
                            plot(ac, which="profile", profileControl=list(parameter="spice"))
                        } else if (input$type =="spiciness profile" && input$applyQC == TRUE && input$focus == "All") {
                            plot(clean, which="profile", profileControl=list(parameter="spice"))
                        } else if (input$type == "spiciness profile" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 == nchar(input$cycle)) {
                            plot(cid, which="profile", profileControl=list(parameter="spice"))
                        } else if (input$type =="spiciness profile" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == FALSE ) {
                            plot(cid, which="profile", profileControl=list(parameter="spice"), col="lightgray")
                            points(unlist(cc[["spice"]]), unlist(cc[["pressure"]]), col="black", pch=20)
                        } else if (input$type =="spiciness profile" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == TRUE ) {
                            plot(cc, which="profile", profileControl=list(parameter="spice"))
                        }

                        if (input$type =="temperature profile" && input$applyQC == FALSE && input$focus == "All") {
                            plot(argos, which="profile", profileControl=list(parameter="temperature"))
                        } else if (input$type =="temperature profile" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 == nchar(input$cycle)) {
                            plot(aid, which="profile", profileControl=list(parameter="temperature"))
                        } else if (input$type =="temperature profile" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == FALSE ) {
                            plot(aid, which="profile", profileControl=list(parameter="temperature"), col="lightgray")
                            points(unlist(ac[["temperature"]]), unlist(ac[["pressure"]]), col="black", pch=20)
                        } else if (input$type =="temperature profile" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == TRUE ) {
                            plot(ac, which="profile", profileControl=list(parameter="temperature"))
                        } else if (input$type =="temperature profile" && input$applyQC == TRUE && input$focus == "All") {
                            plot(clean, which="profile", profileControl=list(parameter="temperature"))
                        } else if (input$type == "temperature profile" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 == nchar(input$cycle)) {
                            plot(cid, which="profile", profileControl=list(parameter="temperature"))
                        } else if (input$type =="temperature profile" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == FALSE ) {
                            plot(cid, which="profile", profileControl=list(parameter="temperature"), col="lightgray")
                            points(unlist(cc[["temperature"]]), unlist(cc[["pressure"]]), col="black", pch=20)
                        } else if (input$type =="temperature profile" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == TRUE ) {
                            plot(cc, which="profile", profileControl=list(parameter="temperature"))
                        }


                                        # Start of histograms

                        if (input$type =="pressure histogram" && input$applyQC == FALSE && input$focus == "All") {
                            p <- unlist(argos[["pressure"]])
                            pmean <- mean(p, na.rm=TRUE)
                            psd <- sd(p, na.rm=TRUE)
                            mean(p, na.rm=TRUE)
                            hist(p, breaks=100, main="Histogram of unflagged values", xlab="Pressure [dbar]")
                            abline(v=pmean + psd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD), lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=pmean + psd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
                        } else if (input$type =="pressure histogram" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 == nchar(input$cycle)) {
                            p <- unlist(aid[["pressure"]])
                            pmean <- mean(p, na.rm=TRUE)
                            psd <- sd(p, na.rm=TRUE)
                            mean(p, na.rm=TRUE)
                            hist(p, breaks=100, main="Histogram of unflagged values", xlab="Pressure [dbar]")
                            abline(v=pmean + psd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD), lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=pmean + psd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)

#New here
                        } else if (input$type =="pressure histogram" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == FALSE) {
                            ## FIXME: make cycle a different color on top of histogram
                            p <- unlist(aid[["pressure"]])
                            pmean <- mean(p, na.rm=TRUE)
                            psd <- sd(p, na.rm=TRUE)
                            mean(p, na.rm=TRUE)
                            hist(p, breaks=100, main="Histogram of unflagged values", xlab="Pressure [dbar]")
                            abline(v=pmean + psd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD), lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=pmean + psd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)

                        } else if (input$type =="pressure histogram" && input$applyQC == FALSE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == TRUE ) {
                            p <- unlist(ac[["pressure"]])
                            pmean <- mean(p, na.rm=TRUE)
                            psd <- sd(p, na.rm=TRUE)
                            mean(p, na.rm=TRUE)
                            hist(p, breaks=100, main="Histogram of unflagged values", xlab="Pressure [dbar]")
                            abline(v=pmean + psd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD), lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=pmean + psd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
# End new
                        } else if (input$type =="pressure histogram" && input$applyQC == TRUE && input$focus == "All") {
                            pc <- unlist(clean[["pressure"]])
                            pcmean <- mean(pc, na.rm=TRUE)
                            pcsd <- sd(pc, na.rm=TRUE)
                            mean(pc, na.rm=TRUE)
                            hist(pc, breaks=100, main="Histogram of unflagged values", xlab="Pressure [dbar]")
                            abline(v=pcmean + pcsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD), lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=pcmean + pcsd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
                        } else if (input$type == "pressure histogram" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 == nchar(input$cycle)) {
                            pc <- unlist(cid[["pressure"]])
                            pcmean <- mean(pc, na.rm=TRUE)
                            pcsd <- sd(pc, na.rm=TRUE)
                            mean(pc, na.rm=TRUE)
                            hist(pc, breaks=100, main="Histogram of unflagged values", xlab="Pressure [dbar]")
                            abline(v=pcmean + pcsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD), lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=pcmean + pcsd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
                            ## New here
                        } else if (input$type =="pressure histogram" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == FALSE ) {
                            ## FIXME: cycle color on top again
                            pc <- unlist(cid[["pressure"]])
                            pcmean <- mean(pc, na.rm=TRUE)
                            pcsd <- sd(pc, na.rm=TRUE)
                            mean(pc, na.rm=TRUE)
                            hist(pc, breaks=100, main="Histogram of unflagged values", xlab="Pressure [dbar]")
                            abline(v=pcmean + pcsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD), lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=pcmean + pcsd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
                        } else if (input$type =="pressure histogram" && input$applyQC == TRUE && 0 != nchar(input$ID) && 0 != nchar(input$cycle) && input$cyclePlot == TRUE ) {
                            pc <- unlist(cc[["pressure"]])
                            pcmean <- mean(pc, na.rm=TRUE)
                            pcsd <- sd(pc, na.rm=TRUE)
                            mean(pc, na.rm=TRUE)
                            hist(pc, breaks=100, main="Histogram of unflagged values", xlab="Pressure [dbar]")
                            abline(v=pcmean + pcsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD), lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=pcmean + pcsd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
                        }
                        ## End pressure, now salinity histogram



                        if (input$type =="salinity histogram" && input$applyQC == FALSE && input$focus == "All") {
                            SA <- unlist(argos[["SA"]])
                            SAmean <- mean(SA, na.rm=TRUE)
                            SAsd <- sd(SA, na.rm=TRUE)
                            hist(SA, breaks=100, main="Histogram of unflagged values", xlab="Absolute Salinity")
                            abline(v=SAmean + SAsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD),lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=SAmean + SAsd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
                        } else if (input$type =="salinity histogram" && input$applyQC == FALSE && 0 != nchar(input$ID)) {
                            SA <- unlist(aid[["SA"]])
                            SAmean <- mean(SA, na.rm=TRUE)
                            SAsd <- sd(SA, na.rm=TRUE)
                            hist(SA, breaks=100, main="Histogram of unflagged values", xlab="Absolute Salinity")
                            abline(v=SAmean + SAsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD),lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=SAmean + SAsd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
                        } else if (input$type =="salinity histogram" && input$applyQC == TRUE && input$focus == "All") {
                            SA <- unlist(clean[["SA"]])
                            SAmean <- mean(SA, na.rm=TRUE)
                            SAsd <- sd(SA, na.rm=TRUE)
                            ## FIXME: is this unflagged?
                            hist(SA, breaks=100, main="Histogram of unflagged values", xlab="Absolute Salinity")
                            abline(v=SAmean + SAsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD),lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=SAmean + SAsd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
                        } else if (input$type == "salinity histogram" && input$applyQC == TRUE && 0 != nchar(input$ID)) {
                            SA <- unlist(cid[["SA"]])
                            SAmean <- mean(SA, na.rm=TRUE)
                            SAsd <- sd(SA, na.rm=TRUE)
                            ## FIXME: is this unflagged?
                            hist(SA, breaks=100, main="Histogram of unflagged values", xlab="Absolute Salinity")
                            abline(v=SAmean + SAsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD),lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=SAmean + SAsd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
                        }

                        if (input$type =="temperature histogram" && input$applyQC == FALSE && input$focus == "All") {
                            CT <- unlist(argos[["CT"]])
                            CTmean <- mean(CT, na.rm=TRUE)
                            CTsd <- sd(CT, na.rm=TRUE)
                            hist(CT, breaks=100, main="Histogram of unflagged values", xlab="Conservative Temperature")
                            abline(v=CTmean + CTsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD),lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=CTmean + CTsd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
                        } else if (input$type =="temperature histogram" && input$applyQC == FALSE && 0 != nchar(input$ID)) {
                            CT <- unlist(aid[["CT"]])
                            CTmean <- mean(CT, na.rm=TRUE)
                            CTsd <- sd(CT, na.rm=TRUE)
                            hist(CT, breaks=100, main="Histogram of unflagged values", xlab="Conservative Temperature")
                            abline(v=CTmean + CTsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD),lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=CTmean + CTsd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
                        } else if (input$type =="temperature histogram" && input$applyQC == TRUE && input$focus == "All") {
                            CTc <- unlist(clean[["CT"]])
                            CTcmean <- mean(CTc, na.rm=TRUE)
                            CTcsd <- sd(CTc, na.rm=TRUE)
                            hist(CTc, breaks=100, main="Histogram of unflagged values", xlab="Conservative Temperature")
                            abline(v=CTcmean + CTcsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD),lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=CTcmean + CTcsd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
                        } else if (input$type == "temperature histogram" && input$applyQC == TRUE && 0 != nchar(input$ID)) {
                            CTc <- unlist(cid[["CT"]])
                            CTcmean <- mean(CTc, na.rm=TRUE)
                            CTcsd <- sd(CTc, na.rm=TRUE)
                            hist(CTc, breaks=100, main="Histogram of unflagged values", xlab="Conservative Temperature")
                            abline(v=CTcmean + CTcsd * c(-3, 0, 3), col=c(colHist3SD, colHistMean, colHist3SD),lwd=1.4)
                            mtext(text=c(expression(mu-3*sigma), expression(mu), expression(mu+3*sigma)),
                                  at=CTcmean + CTcsd * c(-3, 0, 3),
                                  col=c(colHist3SD, colHistMean, colHist3SD), side=3, cex=1.2)
                        }


                    })

# Creating double-click

  shiny::observeEvent(input$dblclick,
                        {
                            ##FIXME: Right now, message gives salinity and temperature. Need to make it recognize when a point is there
                            x <- input$dblclick$x
                            y <- input$dblclick$y
                            msg <- sprintf("Salinity = %.3f g/kg, Temperature =  %.3f degC", x,y)
                            shiny::showNotification(shiny::HTML(msg), duration=NULL)
                        })

})

#' Interactive app for analyzing Argo QC
#'
#' The GUI permits user to analyze QC of Argo data. As of 2020-02-05, this was still in early stages of development.
#'
#' @param debug integer value that controls how much information `mapApp()` prints
#' to the console as it works.  The default value of 0 leads to a fairly limited
#' amount of printing, while higher values lead to more information. This information
#' can be helpful in diagnosing problems or bottlenecks.
#'
#' @examples
#' if (interactive()) {
#'     library(argoFloats)
#'     qcApp()
#' }
#'
#' @author Jaimie Harbin
#' @importFrom shiny shinyApp shinyOptions
#' @export
qcApp <- function(debug=0)
{
    debug <- as.integer(max(0, min(debug, 3))) # put in range from 0 to 3
    if (!requireNamespace("shiny", quietly=TRUE))
        stop("must install.packages(\"shiny\") for this to work")
    print(shiny::shinyApp(ui=uiQCApp, server=QCAppserver))
}
