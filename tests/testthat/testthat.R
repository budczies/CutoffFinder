library(testthat)

context("Test files existence")
expect_true(file.exists(file.path(system.file("inst/exampledata",
                                              package="CutoffFinder"),
                                  "breastcancer_GSE2034.txt")))
# expect_true(file.exists(file.path(system.file("inst",
#                                               package="CutoffFinder"),
#                                   "breastcancer_GSE2034.txt")))
#"breastcancer_GSE7390.txt"
#"breastcancer_GSE11121.txt"

test_that("Complete Analysis (Generates all plots)", {
      baseDir <- file.path(system.file("inst/exampledata",
                                       package="CutoffFinder"))
      filepath <- file.path(baseDir,
                            "breastcancer_GSE2034.txt")
      myData <- read.csv(file = filepath,
                         check.names=FALSE,
                         sep = "\t")

#      type <- "none"
      type <- "distribution"
      biomarker <- myData[["ESR1 217163_at"]]
      outcome <- myData[["ER_IHC"]]
      time <- myData[["dmfs_time"]]
      event <- myData[["dmfs_event"]]
      cutoff <- NULL #in case cutoff value is filled..
      threshold <- NULL #Related to the minimum specificity or sensitivity if selected

      histogramPlots <- c("histogram")
      outcomePlots <- c("OR", "ROC", "waterfall")
      survivalPlots <- c("HR", "time", "kaplanmeier")
      requestedPlots <- c(histogramPlots,
                          outcomePlots,
                          survivalPlots)

      nmin=10
      get.cutoff(type = type,
                        filename = filepath,
                        biomarker = biomarker,
                        outcome = outcome,
                        time = time,
                        event = event,
                        cutoff = NULL,
                        threshold = NULL,
                        plots = requestedPlots,
                        nmin = nmin)
      resultFiles <- list.files(baseDir,
                                pattern = paste0("*", requestedPlots, collapse = "|", ".jpg$"))
      expect_equal(length(requestedPlots), length(resultFiles))
})


test_that("Partial Analysis (Generates outcome related plots)", {
  baseDir <- file.path(system.file("inst/exampledata",
                                   package="CutoffFinder"))
  filepath <- file.path(baseDir,
                        "breastcancer_GSE2034.txt")
  myData <- read.csv(file = filepath,
                     check.names=FALSE,
                     sep = "\t")

  #      type <- "none"
  type <- "distribution"
  biomarker <- myData[["ESR1 217163_at"]]
  outcome <- myData[["ER_IHC"]]
  cutoff <- NULL #in case cutoff value is filled..
  threshold <- NULL #Related to the minimum specificity or sensitivity if selected

  histogramPlots <- c("histogram")
  outcomePlots <- c("OR", "ROC", "waterfall")
  requestedPlots <- c(histogramPlots,
                      outcomePlots)

  nmin=10
  get.cutoff(type = type,
                    filename = filepath,
                    biomarker = biomarker,
                    outcome = outcome,
                    cutoff = NULL,
                    threshold = NULL,
                    plots = requestedPlots,
                    nmin = nmin)
  resultFiles <- list.files(baseDir,
                            pattern = paste0("*", requestedPlots, collapse = "|", ".jpg$"))
  expect_equal(length(requestedPlots), length(resultFiles))
})


test_that("Partial Analysis (Generates survival related plots)", {
  baseDir <- file.path(system.file("inst/exampledata",
                                   package="CutoffFinder"))
  filepath <- file.path(baseDir,
                        "breastcancer_GSE2034.txt")
  myData <- read.csv(file = filepath,
                     check.names=FALSE,
                     sep = "\t")

  #      type <- "none"
  type <- "distribution"
  biomarker <- myData[["ESR1 217163_at"]]
  time <- myData[["dmfs_time"]]
  event <- myData[["dmfs_event"]]
  cutoff <- NULL #in case cutoff value is filled..
  threshold <- NULL #Related to the minimum specificity or sensitivity if selected

  histogramPlots <- c("histogram")
  survivalPlots <- c("HR", "time", "kaplanmeier")
  requestedPlots <- c(histogramPlots,
                      survivalPlots
  )

  nmin=10
  get.cutoff(type = type,
                    filename = filepath,
                    biomarker = biomarker,
                    time = time,
                    event = event,
                    cutoff = NULL,
                    threshold = NULL,
                    plots = requestedPlots,
                    nmin = nmin)
  resultFiles <- list.files(baseDir,
                            pattern = paste0("*", requestedPlots, collapse = "|", ".jpg$"))
  expect_equal(length(requestedPlots), length(resultFiles))
})
