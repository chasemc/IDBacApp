#' beginWithMZ
#'
#' @param id NA
#'
#' @return NA
#'
#'
convertMZ_UI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Starting with mzML or mzXML Data:"),
    p(strong("1: Enter a name for this new experiment")),
    p("This will become a filename, non-valid characters will be removed."),
    p("Hint: Intead of a space, use \"_\"."),
    textInput(ns("newExperimentName"),
      label = "",
      width = "50%",
      placeholder = "Enter Experiment Name Here"
    ),
    verbatimTextOutput(ns("newExperimentNameText"),
      placeholder = TRUE
    ),
    tags$hr(size = 20),
    p(strong("2: Click to select the location of your mzML files"),
      align = "center"
    ),
    actionButton(ns("mzmlRawFileDirectory"),
      label = "Raw Data Folder"
    ),
    verbatimTextOutput(ns("mzmlRawFileDirectorytext"),
      placeholder = TRUE
    ),
    tags$hr(size = 20),
    p("Samples will be named according to the file name of the provided files"),
    br(),
    p(strong("4:", "Click \"Process Data\" to begin spectra conversion.")),
    actionButton(ns("runMsconvert"),
      label = "Process Data"
    ),
    tags$hr(size = 20)
  )
}

#' convertMZ_Server
#'
#' @param input module
#' @param output module
#' @param session module
#' @param sqlDirectory sqlDirectory
#' @param availableExperiments availableExperiments
#' @param ... advanced arguments for MALDIquant, see [IDBacApp::processSmallMolSpectra()] and/or [IDBacApp::processProteinSpectra()]
#'
#' @return .
#'
#'
convertMZ_Server <- function(input,
                             output,
                             session,
                             sqlDirectory,
                             availableExperiments,
                             ...) {
  # Reactive variable returning the user-chosen location of the raw MALDI files as string
  #----
  mzmlRawFilesLocation <- reactive({
    if (input$mzmlRawFileDirectory > 0) {
      loc <- choose_dir()
      if (!is.na(loc)) {
        return(loc)
      }
    }
  })

  output$newExperimentNameText <- renderText({
    a <- gsub(" ", "", sanitize(input$newExperimentName))
    if (a == "") {
      "Once entered, the filename-friendly version of the entered name will appear here once. \n
      This will be the version of your experiment name that is saved."
    } else {
      a
    }
  })

  # Creates text showing the user which directory they chose for raw files
  #----
  output$mzmlRawFileDirectorytext <- renderText({
    if (is.null(mzmlRawFilesLocation())) {
      return("No Folder Selected")
    } else {
      folders <- NULL
      # Get the folders contained within the chosen folder. Timer was taken out.
      foldersInFolder <- tryCatch(
        find_mz_files(mzmlRawFilesLocation(),
          recursive = TRUE,
          full = FALSE
        ),
        error = function(x) paste("Timed out"),
        finally = function(x) x
      )
      if (foldersInFolder == "Timed out") {
        return("Timed out looking for mzML/mzXML files. This can happen if the folder you
             selected has lots of folders within it... because IDBac looks through all
             of them for mzML/mzXML files.")
      } else {
        for (i in 1:length(foldersInFolder)) {
          # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
          folders <- paste0(
            folders,
            "\n",
            basename(foldersInFolder[[i]])
          )
        }
        return(folders)
      }
    }
  })
  # make sure the name is ok as a file name
  sanity <- reactive({
    a <- sanitize(input$newExperimentName)
    gsub(" ", "", a)
  })

  observeEvent(input$runMsconvert, {
    req(!is.null(mzmlRawFilesLocation()))
    req(!is.null(sanity()))
    req(sanity() != "")
    popup3()
    mzFilePaths <- find_mz_files(mzmlRawFilesLocation(),
      recursive = TRUE,
      full = TRUE
    )

    idbac_create(
      fileName = sanity(),
      filePath = sqlDirectory$sqlDirectory
    )
    idbacPool <- idbac_connect(
      fileName = sanity(),
      filePath = sqlDirectory$sqlDirectory
    )[[1]]

    db_from_mzml(
      mzFilePaths = mzFilePaths,
      sampleIds = base::basename(tools::file_path_sans_ext(mzFilePaths)),
      idbacPool = idbacPool,
      acquisitionInfo = NULL,
      ...
    )
    pool::poolClose(idbacPool)
    popup4()
    # Update available experiments
    availableExperiments$db <- tools::file_path_sans_ext(list.files(sqlDirectory$sqlDirectory,
      pattern = ".sqlite",
      full.names = FALSE
    ))
  })
}
