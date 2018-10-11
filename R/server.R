#' CutoffFinder Shinyapp
#' ShinyApp from \code{\link{cutoff}}
#'
#' @author Cristiano Oliveira, \email{cristiano.oliveira@iph.med-heidelberg.de}
#' @seealso \code{\link{cutoff}}
#' @keywords cutoff

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
#' @importFrom shiny h2
#' @importFrom shiny withTags
#' @importFrom shinyBS addTooltip
#' @importFrom shinyjs html
#' @importFrom stringr str_match
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @importFrom shiny hideTab
#' @importFrom shiny showTab

#' @noRd
addTooltipHelper <- function(session,
                             id,
                             title,
                             placement = "bottom",
                             trigger = "hover",
                             options = list(delay = list(show=1000, hide=1000))) {
  shinyBS::addTooltip(session = session,
                      id = id,
                      title = title,
                      placement = placement,
                      trigger = trigger
                      #                      ,
                      #                      options = options
  )
}

################################################################################
# shinyServer
################################################################################
#' @noRd
CutoffFinderServer <- function(input, output, session) {
  shortNotificationDuration <- 3
  longNotificationDuration <- 5
  LABEL_NO_DATA <- "(no data)"
  inputDirectory <- CreateSessionInputDirectory(session)
  myData <- NULL

  # notificationsTootip <<- c()

  hideTab(inputId = "inTabset", target = "tsvFile", session = session)
  hideTab(inputId = "inTabset", target = "analysis", session = session)

  # shinyBS::addTooltip(session = session, id = "tsvFilepathHelper",
  #                     #          bsTooltip(id = "tsvFilepathHelper",
  #                     title = "Input a tab separated value file. It can have any extension as long as it is a tab separated",
  #                     #                    placement = "right",
  #                     placement = "bottom",
  #                     trigger = "hover",
  #                     #                    options = NULL,
  #                     options = list(delay = list(show=1000, hide=3000)))


  addTooltipHelper(session = session,
                   id = "tsvFilepathHelper",
                   title = "Input a tab separated value file. It can have any extension as long as it is a tab separated")
  addTooltipHelper(session = session,
                   id = "biomarkerHelper",
                   title = "Select the biomarker")
  addTooltipHelper(session = session,
                   id = "outcomeHelper",
                   title = "Select an outcome column")
  addTooltipHelper(session = session,
                   id = "survivalTimeHelper",
                   title = "Select a time column associated with survival event")
  addTooltipHelper(session = session,
                   id = "survivalEventHelper",
                   title = "Select an event column associated with survival time")
  addTooltipHelper(session = session,
                   id = "methodHelper",
                   title = "Select the method")
  addTooltipHelper(session = session,
                   id = "limit_outcome_sensitivity",
                   title = "Select the limit of the outcome sensitivity")
  addTooltipHelper(session = session,
                   id = "limit_outcome_specificity",
                   title = "Select the limit of the outcome sensitivity")
  addTooltipHelper(session = session,
                   id = "cutoffValue",
                   title = "Select the manual cutt off value")
  addTooltipHelper(session = session,
                   id = "Run",
                   title = "Run tooltip")


  observeEvent(input$tsvFilepath, {
    if (!is.null(input$tsvFilepath)) {
      file.remove(list.files(path = inputDirectory, full.names = TRUE, pattern = "*"))
      fixUploadedFilesNames(input$tsvFilepath, inputDirectory)
      myData <- read.csv(file = UploadedFilepaths(input$tsvFilepath, inputDirectory),
                         check.names=FALSE,
                         sep = "\t")
      output$importedTsvFilepath <- renderDataTable(FixDataTable(input$tsvFilepath,
                                                                 inputDirectory,
                                                                 type = file_ext(input$tsvFilepath$datapath)),
                                                    options = list(dom = "", searching = FALSE))
      # To edit number of options just use this vector
      # the 'All' option is added by default
      # and the first options is selected by default
      availableRowsInComboBoxOption <- c(10, 25, 50, 100)
      availableRowsInComboBoxOptionsExtendedWithAll <- c(availableRowsInComboBoxOption, -1)
      labelsForAvailableRowsInComboBoxOptionsExtendedWithAll <- c(
        sapply(availableRowsInComboBoxOption, toString), 'All'
      )
      output$tsvFile <- renderDataTable(myData,
                                        options = list(
                                          lengthMenu = list(availableRowsInComboBoxOptionsExtendedWithAll,
                                                            labelsForAvailableRowsInComboBoxOptionsExtendedWithAll),
                                          pageLength = availableRowsInComboBoxOptionsExtendedWithAll[1],
                                          autoWidth = TRUE,
                                          scrollX = TRUE,
                                          fillContainer = TRUE,
                                          searching = TRUE))

      # To avoid updating the input file and keep an old value..
      updateNumericInput(session, "cutoffValue", value = 0)

      output$biomarkerHelper = renderUI({ #creates State select box object called in ui
        selectInput(inputId = "biomarker", #name of input
                    label = "Biomarker:", #label displayed in ui
                    choices = c(LABEL_NO_DATA, colnames(myData))
        )
      })

      output$outcomeHelper = renderUI({ #creates State select box object called in ui
        selectInput(inputId = "outcome", #name of input
                    label = "Outcome:", #label displayed in ui
                    choices = c(LABEL_NO_DATA, colnames(myData))
        )
      })

      output$survivalTimeHelper = renderUI({ #creates State select box object called in ui
        selectInput(inputId = "survivalTime", #name of input
                    label = "Survival Time:", #label displayed in ui
                    choices = c(LABEL_NO_DATA, colnames(myData))
        )
      })

      output$survivalEventHelper = renderUI({ #creates State select box object called in ui
        selectInput(inputId = "survivalEvent", #name of input
                    label = "Survival Event:", #label displayed in ui
                    choices = c(LABEL_NO_DATA, colnames(myData))
        )
      })
    }


    showTab(inputId = "inTabset", target = "tsvFile", session = session)
    hideTab(inputId = "inTabset", target = "analysis", session = session)

  })

  observeEvent(input$biomarker, {
    if (!is.null(input$tsvFilepath)) {
      myData <- read.csv(file = UploadedFilepaths(input$tsvFilepath,
                                                  inputDirectory),
                         check.names=FALSE,
                         sep = "\t")
      biomarker <- myData[[input$biomarker]]
      if (all(is.numeric(biomarker))) {
        maxCutoff <- max(biomarker)
        meanCutOff <- round(mean(biomarker))
        minCutoff <- min(biomarker)
        updateNumericInput(session, "cutoffValue",
                           value = meanCutOff,
                           min = minCutoff,
                           max = minCutoff)}
    } else {
      notificationId <- showNotification(paste("Selection of the biomarker is required"),
                       duration = longNotificationDuration)
      # notificationsTootip <- c(notificationsTootip, notificationId)
      # print("and now biomarker!?!")
      # print(notificationsTootip)
    }

    updateTabsetPanel(session, "inTabset", selected = "tsvFile")
    hideTab(inputId = "inTabset", target = "analysis", session = session)

  })

  onSessionEnded(function() {
    cat("Session Ended\n")
    success <- unlink(GetSessionDirectory(session),
                      recursive = T)
    if (success) {
      print("Delete temporary folder sucessfully")
    } else {
# TODO why is this not working..
#      print("Failed to delete temporary folder")
#      print(GetSessionDirectory(session))
    }
  })

  observeEvent(input$Run, {

    #print("before")
    #print(notificationsTootip)
    # lapply(notificationsTootip, removeNotification)
    # notificationsTootip <<- c()
    # print("after")
    # print(notificationsTootip)

    if (is.null(input$tsvFilepath)) {
      notificationId <- showNotification(paste("Please upload required file"), duration = shortNotificationDuration)
      # print(notificationId)
      # print("and now notificationId!?!")
      # print(notificationId)
      # notificationsTootip <<- c(notificationsTootip, notificationId)
      # print("and now upload!?!")
      # print(notificationsTootip)

      return()
    }
    filepath <- UploadedFilepaths(input$tsvFilepath, inputDirectory)
    myData <- read.csv(file = filepath,
                       check.names=FALSE,
                       sep = "\t")
    if ((input$biomarker == LABEL_NO_DATA) | (!is.numeric(myData[[input$biomarker]]))) {
      showNotification(paste("Please select a valid biomarker"), duration = shortNotificationDuration)
      return()
    }
    if ((input$survivalTime != LABEL_NO_DATA) & (input$survivalEvent == LABEL_NO_DATA)) {
      showNotification(paste("If you select a survival time, you need to select a survival event too"), duration = longNotificationDuration)
      return()
    }
    if ((input$survivalTime == LABEL_NO_DATA) & (input$survivalEvent != LABEL_NO_DATA)) {
      showNotification(paste("If you select a survival event, you need to select a time event too"), duration = longNotificationDuration)
      return()
    }
    if ((input$outcome == LABEL_NO_DATA) & grepl("outcome", input$method)) {
      showNotification(paste("If you select a method that to operate on the outcome, you need to select an outcome too."), duration = longNotificationDuration)
      return()
    }
    if (((input$survivalTime == LABEL_NO_DATA) | (input$survivalEvent == LABEL_NO_DATA)) &
        grepl("survival", input$method)) {
      showNotification(paste("If you select a method that based on survival data, you need to select a combination of survival time and event too."), duration = longNotificationDuration)
      return()
    }
    # Empty temporary files
    unlink(
      list.files(path = GetSessionInputDirectory(session),
                 pattern = "*.jpg",
                 all.files = TRUE,
                 full.names = TRUE,
                 recursive = TRUE),
      recursive = TRUE)
    updateTabsetPanel(session, "inTabset", selected = "analysis")
    withProgress(message = 'Analysis in progress', {
      type <- input$method
      filename <- UploadedFilepaths(input$tsvFilepath, inputDirectory)
      biomarker <- myData[[input$biomarker]]
      names(biomarker) <- input$biomarker

      NONE_VALUE_SELECTION = NULL
      outcome = NONE_VALUE_SELECTION
      if (input$outcome != LABEL_NO_DATA) {
        outcome <- myData[[input$outcome]]
      }

      time <- NONE_VALUE_SELECTION
      if (input$survivalTime != LABEL_NO_DATA) {
        time <- myData[[input$survivalTime]]
      }

      event <- NONE_VALUE_SELECTION
      if (input$survivalEvent != LABEL_NO_DATA) {
        event <- myData[[input$survivalEvent]]
      }

      #in case cutoff value is filled..
      cutoff <- if (input$method == 'manual') {
        input$cutoffValue
      } else {
        NULL
      }

      #     Related to the minimum specificity or sensitivity if selected
      threshold <- if (input$method == 'outcome_sensitivity') {
        input$limit_outcome_sensitivity
      } else if (input$method == 'outcome_specificity') {
        input$limit_outcome_specificity
      } else {
        NULL
      }

      histogramPlots <- c("histogram")
      outcomePlots <- c("OR", "ROC", "waterfall")
      survivalPlots <- c("HR", "time", "kaplanmeier")

      # The requested plots depend on the selection of other parameters..
      plots <- histogramPlots
      if (input$outcome != LABEL_NO_DATA) {
        plots <- c(plots, outcomePlots)
      }
      if ((input$survivalTime != LABEL_NO_DATA)
          && (input$survivalEvent != LABEL_NO_DATA)) {
        plots <- c(plots, survivalPlots)
      }

      nmin=10
      res <- get.cutoff(type = type,
                        filename = filename,
                        biomarker = biomarker,
                        outcome = outcome,
                        time = time,
                        event = event,
                        cutoff = cutoff,
                        threshold = threshold,
                        plots = plots,
                        nmin = nmin)

      ##########################################################################
      # Display HTML outputted by function..
      ##########################################################################
      # Fix malformed HTML
      outputTextFixed <- gsub("<br>", "<br/>", res[[1]])
      outputTextFixed <- gsub("&quot", "&quot;", outputTextFixed)

      html("htmlPlaceholder", "")
      insertUI(
        selector = '#htmlPlaceholder',
        ui = HTML(outputTextFixed)
      )

      ##########################################################################
      # Display plots
      ##########################################################################
      filename <- basename(UploadedFilepaths(input$tsvFilepath, inputDirectory))
      myPlotsFilepaths <- list.files(path = inputDirectory,
                                     full.names = TRUE,
                                     pattern = "*.jpg$")
      RemovePlots <- function(placeHolder) {
        removeUI(selector = paste0("#", placeHolder, "Helper"),
                 multiple = TRUE,
                 immediate = TRUE
        )
      }
      DisplayPlots <- function(placeHolder, placeHolderLabel, myPlotsFilepaths) {
        insertUI(
          selector = '#plots',
          ui = withTags(
            div(id = paste0(placeHolder, "Helper"), style = "margin-top: 10px"
                ,h2(placeHolderLabel)
                ,uiOutput(placeHolder, width = "50%", height="50%")
            )
          )
        )

        fileIdentifiers <- str_match(string = myPlotsFilepaths, pattern = '.+_([^\\.]+).jpg$' )[,2]
        output[[placeHolder]] <- renderUI({
          plot_output_list <- lapply(1:length(myPlotsFilepaths), function(i) {
            plotname <- paste("plot", fileIdentifiers[i], sep = "_")
            plotOutput(plotname, height = "100%", width = "100%")
          })
          # Convert the list to a tagList - this is necessary for the list of items
          # to display properly.
          do.call(tagList, plot_output_list)
        })

        # Call renderPlot for each one. Plots are only actually generated when they
        # are visible on the web page.
        #  for (i in 1:max_plots) {
        for (i in 1:length(myPlotsFilepaths)) {
          # Need local so that each item gets its own number. Without it, the value
          # of i in the renderPlot() will be the same across all instances, because
          # of when the expression is evaluated.
          local({
            my_i <- i
            plotname <- paste("plot", fileIdentifiers[i], sep = "_")
            myFilename <- myPlotsFilepaths[i]
            output[[plotname]] <- renderImage({
              filename <- myFilename
              # Return a list containing the filename
              list(src = filename, width="100%", height ="100%")
            }, deleteFile = FALSE)
          })
        }
      }
      mandatoryFilePatterns <-  paste(histogramPlots, collapse = "|")
      outcomeFilePatterns <- paste(outcomePlots, collapse = "|")
      survivalFilePatterns <- paste(survivalPlots, collapse = "|")

      mandatoryPlotsFilepaths <- myPlotsFilepaths[grepl(mandatoryFilePatterns, myPlotsFilepaths)]
      outcomePlotsFilepaths <- myPlotsFilepaths[grepl(outcomeFilePatterns, myPlotsFilepaths)]
      survivalPlotsFilepaths <- myPlotsFilepaths[grepl(survivalFilePatterns, myPlotsFilepaths)]

      # To sort the order of appearance of the survival plots..
      if (length(survivalPlotsFilepaths) > 1) {
        survivalPlotsFilepaths <- survivalPlotsFilepaths[c(which(grepl("HR", survivalPlotsFilepaths)),
                                                           which(grepl("time", survivalPlotsFilepaths)),
                                                           which(grepl("kaplanmeier", survivalPlotsFilepaths)))]
      }
      RemovePlots("plotsMandatory")
      RemovePlots("plotsOutcome")
      RemovePlots("plotsSurvival")

      if (length(mandatoryPlotsFilepaths) > 0) {
        DisplayPlots("plotsMandatory", "Histogram", mandatoryPlotsFilepaths)
      }
      if (length(outcomePlotsFilepaths) > 0) {
        DisplayPlots("plotsOutcome", "Outcome", outcomePlotsFilepaths)
      }
      if (length(survivalPlotsFilepaths) > 0) {
        DisplayPlots("plotsSurvival", "Survival", survivalPlotsFilepaths)
      }
    })


#    showTab(inputId = "inTabset", target = "tsvFile", session = session)
    showTab(inputId = "inTabset", target = "analysis", session = session)


  })

  output$downloadData <- myZipDownloadHandler(generatedFilename = "cutoffFinder",
                                              basedir = inputDirectory,
                                              pattern = "*.(jpg|tsv)$")
}
