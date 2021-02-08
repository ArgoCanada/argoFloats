library(shiny)

uiQCApp <- fluidPage(
                shiny::fluidRow(shiny::uiOutput(outputId="UIwidget")),
                shiny::fluidRow(shiny::uiOutput(outputId="UIview")),
                shiny::fluidRow(shiny::uiOutput(outputId="UIsingleQC"))
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
                          "tSincePowerOn(t)"="tSincePowerOn time-series",
                          "C(p)"="conductivity profile",
                          "density(p)"="density profile",
                          "S(p)"="salinity profile",
                          "spiciness(p)"="spiciness profile",
                          "T(p)"="temperature profile",
                          "hist(C)"="conductivity histogram",
                          "hist(p)"="pressure histogram",
                          "hist(S)"="salinity histogram",
                          "hist(T)"="temperature histogram"),
                selected="pressure time-series")),

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
