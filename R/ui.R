#' CutoffFinder Shinyapp
#' ShinyApp from \code{\link{cutoff}}
#'
#' @author Cristiano Oliveira, \email{cristiano.oliveira@iph.med-heidelberg.de}
#' @seealso \code{\link{cutoff}}
#' @keywords cutoff
#'

#' @importFrom shiny renderDataTable
#' @importFrom shiny insertUI
#' @importFrom shiny observeEvent
#' @importFrom shiny updateNumericInput
#' @importFrom shiny renderUI
#' @importFrom shiny showNotification
#' @importFrom shiny onSessionEnded
#' @importFrom shiny updateTabsetPanel
#' @importFrom shiny withProgress
#' @importFrom shiny removeUI
#' @importFrom shiny uiOutput
#' @importFrom shiny tagList
#' @importFrom shiny renderImage
#' @importFrom shiny validate
#' @importFrom shiny selectInput
#' @importFrom shiny plotOutput
#' @importFrom shiny HTML
#' @importFrom shiny div
#' @importFrom shiny withTags
#' @importFrom shinyBS addTooltip
#' @importFrom shinyjs html
#' @importFrom stringr str_match
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @importFrom shiny hideTab
#' @importFrom shiny showTab
#' @importFrom shiny includeHTML
#' @importFrom shiny shinyApp
#' @importFrom shiny shinyUI
#' @importFrom shiny downloadButton
#' @importFrom shiny fluidPage
#' @importFrom shiny textAreaInput
#' @importFrom shiny headerPanel
#' @importFrom shiny mainPanel
#' @importFrom shiny titlePanel
#' @importFrom shiny numericInput
#' @importFrom shiny h3
#' @importFrom shiny sidebarPanel
#' @importFrom shiny htmlOutput
#' @importFrom shiny tabPanel
#' @importFrom shiny fileInput
#' @importFrom shiny sidebarLayout
#' @importFrom shiny dataTableOutput
#' @importFrom shiny tags
#' @importFrom shiny div
#' @importFrom shiny tabsetPanel
#' @importFrom shiny withTags
#' @importFrom shiny icon
#' @importFrom shiny p
#' @importFrom shiny actionButton
#' @importFrom shiny conditionalPanel
#' @importFrom shinyBS bsTooltip
#' @importFrom shinyBS tipify
#' @importFrom shinyjs useShinyjs

################################################################################
#shinyUI
################################################################################
#' @noRd
CutoffFinderUI <- function() {
  shinyUI(
    fluidPage(
      tags$head(
        tags$link(rel = "shortcut icon",
                  type = "image/x-icon",
                  href = "www/favicon.ico")),
      useShinyjs(),
      headerPanel("CutoffFinder"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(id = "tsvFilepathHelper",
              fileInput(inputId = "tsvFilepath",
                        label="Import tsv file",
                        multiple = FALSE,
                        accept = NULL)),
          # bsTooltip(
          #   id = "tsvFilepathHelper",
          #   title = "Input a tab separated value file. It can have any extension as long as it is a tab separated",
          #   placement = "bottom",
          #   trigger = "hover"
          # ),

#          conditionalPanel(
#            condition = "!is.null(input.tsvFilepath)",
#            condition = "output$csv_import_ready",

          tabPanel("assignmentOfVariables",
                   div(style="display: inline-block;font-weight: bold; width: 180px;",
                       p("Assignment of variables"))),
          htmlOutput("biomarkerHelper"),
          htmlOutput("outcomeHelper"),
          htmlOutput("survivalTimeHelper"),
          htmlOutput("survivalEventHelper"),
          div(id = "methodHelper",
              selectInput("method",
                          "Method for cutoff determination:",
                          c("None" = "none",
                            "Fit of mixture model" = "distribution",
                            "Outcome: significance (Fisher's exact test)" = "outcome_significance",
                            "Outcome: ROC curve (Euclidean distance)" = "outcome_euclidean",
                            "Outcome: ROC curve (Manhattan distance)" = "outcome_manhattan",
                            "Outcome: minimum sensitivity" = "outcome_sensitivity",
                            "Outcome: minimum specificity" = "outcome_specificity",
                            "Survival: significance (log-rank test)" = "survival_significance",
                            "manual cutoff" = "manual"))
          ),
          conditionalPanel(
            condition = "input.method == 'outcome_sensitivity'",
            numericInput(inputId = "limit_outcome_sensitivity",
                         label = "Sensitivity Threshold (%)",
                         value = 80,
                         min = 0,
                         max = 100)
          ),
          conditionalPanel(
            condition = "input.method == 'outcome_specificity'",
            numericInput(inputId = "limit_outcome_specificity",
                         label = "Specificity Threshold (%)",
                         value = 80,
                         min = 0,
                         max = 100)
          ),
          conditionalPanel(
            condition = "input.method == 'manual'",
            numericInput(inputId = "cutoffValue",
                         label = "cutoff value",
                         value = 0)
          ),
          div(actionButton('Run', label = "OK",
                           icon = icon("ok", lib = "glyphicon")),
              style="float:right;"),
          withTags( # hack to fix margin issue..
            div(style = "margin-top: 80px")
          )
#          )
        ),

        mainPanel(
          width = 8,
          tabsetPanel(id = "inTabset",
                      tabPanel('Home',
                               div(style = "margin-top: 30px"),
#                               img(src = "fig1_workflow.jpg"),
#                               www/fig1_workflow.jpg
#                               img(src = "www/fig1_workflow.jpg"),

#                               includeHTML("www/home.html")),
                                includeHTML(system.file("www/home.html", package="CutoffFinder"))),
#                                includeHTML(system.file("extdata/www/home.html", package="CutoffFinder"))),
#                               includeHTML(system.file("www/home.html", package="CutoffFinder"))),
                      tabPanel('TSV file', value = "tsvFile",
                               div(style = "margin-top: 30px"),
                               # dataTableOutput("importedTsvFilepath"),
                               # div(style = "margin-top: 60px"),
                               dataTableOutput("tsvFile")),
                      tabPanel('Analysis', value = "analysis",
                               withTags(
                                 div(class = "someSpace",
                                     h3("  "))
                               ),
                               div(downloadButton('downloadData', 'Download'), style="float:right;"),
                               # HAVE TO KEEP AT LEAST ONE TOOLTIP AT THE CLIENT FOR THIS LIBRARY TOWORK..
                               bsTooltip(id = "downloadData",
                                         title = "Download all data and images",
                                         placement = "bottom",
                                         trigger = "hover",
                                         options = NULL),
                               # Place holder for HTML code outputted by the cutoff script..
                               tags$div(id = 'htmlPlaceholder'),
                               tags$div(id = 'plots'))
          )
        )
      )
    )
  )
}
