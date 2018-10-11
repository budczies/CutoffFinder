
#' @importFrom shiny downloadHandler
#' @importFrom utils zip
################################################################################
# File management helper functions
################################################################################

# TODO improve ( A little fragile... to say the least..)
MoveFileTo <- function(from, to) {
  print(paste("from : ", from))
  print(paste("to : ", to))
  #  todir <- dirname(to)
  todir <- unique(dirname(to))

  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  #  file.rename(from = from,  to = to)

  for (i in seq_along(from)) {
    file.rename(from = from[i],  to = to[i])
  }
}

UploadedFilepaths <- function(x, inputDirectory) {
  return(file.path(inputDirectory, x$name))
}

fixUploadedFilesNames <- function(x, inputDirectory = "") {
  if (is.null(x)) {
    return()
  }
  oldNames = x$datapath
  newNames = UploadedFilepaths(x, inputDirectory)
  MoveFileTo(from = oldNames, to = newNames)
  x$datapath <- newNames
  x
}

GetSessionDirectory <- function(session) {
  sessionDirectory <- file.path(tempdir(), session$token)
#  sessionDirectory <- file.path(getwd(), "tmp", session$token)
  return(sessionDirectory)
}

GetSessionOutputDirectory <- function(session) {
  outputDirectory <- file.path(GetSessionDirectory(session), "output")
#  outputDirectory <- file.path(getwd(), "tmp", session$token, "output")
  return(outputDirectory)
}

CreateSessionOutputDirectory <- function(session) {
  outputDirectory <- GetSessionOutputDirectory(session)
  dir.create(outputDirectory, recursive = TRUE)
  return(outputDirectory)
}

GetSessionInputDirectory <- function(session) {
  intputDirectory <- file.path(GetSessionDirectory(session), "input")
#  intputDirectory <- file.path(getwd(), "tmp", session$token, "input")
  return(intputDirectory)
}

CreateSessionInputDirectory <- function(session) {
  intputDirectory <- GetSessionInputDirectory(session)
  dir.create(intputDirectory, recursive = TRUE)
  return(intputDirectory)
}

EmptySessionOutputDirectory <- function(session) {
  print("temporary directory Contents before deleting...")
  print(paste("tmpdir", GetSessionOutputDirectory(session)))
  print(list.files(GetSessionOutputDirectory(session)))
  unlink(GetSessionOutputDirectory(session), recursive = TRUE)
  CreateSessionOutputDirectory(session)
  print("temporary directory Contents after deleting...")
  print(list.files(GetSessionOutputDirectory(session)))
}

EmptySessionInputDirectory <- function(session) {
  print("temporary directory Contents before deleting...")
  print(paste("tmpdir", GetSessionInputDirectory(session)))
  print(list.files(GetSessionInputDirectory(session)))
  unlink(GetSessionInputDirectory(session), recursive = TRUE)
  CreateSessionInputDirectory(session)
  print("temporary directory Contents after deleting...")
  print(list.files(GetSessionInputDirectory(session)))
}

# This function was copy/pasted from utils:::format.object_size
# To avoid complaints from CRAN check..
my.format.object_size <- function (x, units = "b", ...)
{
  units <- match.arg(units, c("b", "auto", "Kb", "Mb", "Gb",
                              "Tb", "Pb", "B", "KB", "MB", "GB", "TB", "PB", "KiB",
                              "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"))
  if (units == "auto")
    units <- if (x >= 1024^4)
      "Tb"
  else if (x >= 1024^3)
    "Gb"
  else if (x >= 1024^2)
    "Mb"
  else if (x >= 1024)
    "Kb"
  else "b"
  switch(units, b = , B = paste(x, "bytes"), Kb = , KB = paste(round(x/1024,
                                                                     1L), "Kb"), Mb = , MB = paste(round(x/1024^2, 1L), "Mb"),
         Gb = , GB = paste(round(x/1024^3, 1L), "Gb"), Tb = ,
         TB = paste(round(x/1024^4, 1L), "Tb"), Pb = , PB = paste(round(x/1024^5,
                                                                        1L), "Pb"), KiB = paste(round(x/1024, 1L), "KiB"),
         MiB = paste(round(x/1024^2, 1L), "MiB"), GiB = paste(round(x/1024^3,
                                                                    1L), "GiB"), TiB = paste(round(x/1024^4, 1L), "TiB"),
         PiB = paste(round(x/1024^5, 1L), "PiB"), EiB = paste(round(x/1024^6,
                                                                    1L), "EiB"), ZiB = paste(round(x/1024^7, 1L), "ZiB"),
         YiB = paste(round(x/1024^8, 1L), "YiB"))
}



# TODO can be improved.. or replaced if data table were configurable..
FixDataTable <- function(x, inputDirectory, type) {
  newX <- x
  newX$datapath = UploadedFilepaths(x, inputDirectory)
  #    newX$datapath = basename(UploadedFilepaths(x, inputDirectory))
  numberOfFiles <- length(newX$datapath)
  newX$type = rep(type, numberOfFiles)
  if (!is.null(newX$size)) {
    newX$size <- sapply(newX$size, my.format.object_size, "auto")
#    newX$size <- sapply(newX$size, utils::format.object_size, "auto")
#    if (!debug) {
#      if (!pkg.env$debug) {
      datapathColumnIndex <- 4
      newX <- newX[, -c(datapathColumnIndex)]
#    }
  }
  return(newX)
}

myZipDownloadHandler <- function(generatedFilename = "analysis",
                                 basedir,
                                 pattern) {
  return(
    downloadHandler(
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {paste(generatedFilename, "zip", sep = ".")},
      content = function(fname) {
        # TODO is there no other way to avoid this hack?!
        originalWorkingDirectory <- getwd()
        setwd(basedir)
        a <- zip(zipfile = fname,
                 files = c(list.files(path = basedir, full.names = FALSE, pattern = pattern)))
        setwd(originalWorkingDirectory)
      },
      contentType = "application/zip"
    )
  )
}
