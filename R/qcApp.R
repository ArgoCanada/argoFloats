library(shiny)

uiQCApp <- fluidPage(
                shiny::fluidRow(shiny::uiOutput(outputId="UIwidget")),
                shiny::fluidRow(shiny::uiOutput(outputId="UIview")),
                shiny::fluidRow(shiny::uiOutput(outputId="UIsingleQC")),
                shiny::fluidRow(shiny::plotOutput(outputId="plotMap"))

)

QCAppserver <- shinyServer(function(input,output){



                          output$UIwidget <- shiny::renderUI({
                                  shiny::fluidRow(shiny::column(2,shiny::span(shiny::HTML(paste("<b style=\"color:black; margin-left:1em;\">  ","qcApp 0.1","</b>")))),
                                                  shiny::column(2, shiny::actionButton("load", "Load")),
                                                  shiny::column(2, shiny::actionButton("help", "Help")),
                                                  shiny::column(2, shiny::actionButton("code", "Code")))
                          })


                          output$UIview <- shiny::renderUI({
                              ## NOTE: The type and colorBy are the same as oceanglider

                                  shiny::fluidRow(
                                                  shiny::column(2, shiny::selectInput("focus", "focus",choices=c("All", "Single"), selected=c("All"))),
                                                  shiny::column(2, shiny::selectInput("type", "Plot Type",choices=c("TS <T>"="TS",
                          "C(t)"="conductivity time-series",
                          "p(t) <P>"="pressure time-series",
                          "S(t) <S>"="salinity time-series",
                          "spiciness(t)"="spiciness time-series",
                          "T(t)"="temperature time-series",
                         # "C(p)"="conductivity profile",
                          "density(p)"="density profile",
                          "S(p)"="salinity profile",
                          "spiciness(p)"="spiciness profile",
                          "T(p)"="temperature profile",
                          "hist(C)"="conductivity histogram",
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
                selected="(none)")))}

                          )

                          output$UIsingleQC <- shiny::renderUI({
                                  shiny::fluidRow(
                                                  shiny::column(2, shiny::textInput("ID", "Float ID", value="", width="11em")),
                                                  shiny::column(2, shiny::textInput("cycle", "Cycle", value="", width="11em")),
                                                  shiny::column(2, shiny::checkboxGroupInput("qc",
                                                                                             label="Single float QC",
                                                                                             choiceNames=list(shiny::tags$span("applyQC", style="color:black;"),
                                                                                                              shiny::tags$span("showQCTests", style="color:black;"),
                                                                                                              shiny::tags$span("Diagnostics", style="color:black;")),
                                                                                             choiceValues=list("applyQC","showQCTests", "diagnostics"),
                                                                                             inline=TRUE)))
                              }
                          )
                          shiny::observeEvent(input$load,
                                              {
                                                  ## FIXME: Right now the rda is saving to the R directory. I'd like to make this go to the argoFloats directory
                                                  load(rda)
                                                  argos <<- readProfiles(getProfiles(index3))
                        })

                          shiny::observeEvent(input$help,
                        {
                            msg <- shiny::HTML("This GUI is used to analyze QC of Argo profiles.<br><br>The Load button is used to load and read the previously saved rda file of a subset of index from mapApp().The Code button brings up a window showing R code that isolates to the view shown and demonstrates some further operations.<br><br>The Plot type determines which plot should be displayed, with the option to color code by a variety of parameters. If the user changes the focus to Single, the user has the option to input the float ID and cycle of interest, with the option to perform a variety of QC. ")
                            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title="Using this application", size="l"))
                        })
                          
                          shiny::observeEvent(input$code,
                        {
                            msg <- shiny::HTML("FIXME:: This still needs to me coded in")
                            shiny::showModal(shiny::modalDialog(shiny::HTML(msg), title="Using this application", size="l"))
                        })
output$plotMap <- shiny::renderPlot({
                        if(input$type =="pressure time-series") {
                            message("we are at pressure time-series")
                        }
                        if(input$type =="TS") {
                            message("we are at TS")
                            plot(argos, which="TS")
                        }
                        if(input$type =="conductivity time-series") {
                            message("we are at conductivity time-series")
                        }
                        if(input$type =="salinity time-series") {
                            message("we are at salinity time-series")
                        }
                        if(input$type =="spiciness time-series") {
                            message("we are at spiciness time-series")
                        }
                        if(input$type =="temperature time-series") {
                            message("we are at temperature time-series")
                        }
                       # if(input$type =="conductivity profile") {
                       #     message("we are at conductivity profile")
                       # }
                        if(input$type =="density profile") {
                            message("we are at density profile")
                            plot(argos, which="profile", profileControl=list(parameter="sigma0"))
                        }
                        if(input$type =="salinity profile") {
                            message("we are at salinity profile")
                            plot(argos, which="profile", profileControl=list(parameter="SA"))
                        }
                        if(input$type =="spiciness profile") {
                            message("we are at spiciness profile")
                            plot(argos, which="profile", profileControl=list(parameter="spice"))
                        }
                        if(input$type =="temperature profile") {
                            message("we are at temperature profile")
                            plot(argos, which="profile", profileControl=list(parameter="temperature"))
                        }
                        if(input$type =="conductivity histogram") {
                            message("we are at conductivity histogram")
                        }
                        if(input$type =="pressure histogram") {
                            message("we are at pressure histogram")
                        }
                        if(input$type =="salinity histogram") {
                            message("we are at salinity histogram")
                        }
                        if(input$type =="temperature histogram") {
                            message("we are at temperature histogram")
                        }
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
