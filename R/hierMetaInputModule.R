

# Module UI function
hierMetaOutput <- function(id, label = "Sample Metadata Input") {
  # Create a namespace function using the provided id
  ns <- NS(id)


  tagList(tags$style(HTML(".htMenu { z-index: 1051; }")),
    rHandsontableOutput(ns("metaTable"))
  )


}



hierMeta <- function(input, output, session, sampleIDs) {


  createNewLibraryTable <- reactive({

    })



  # Display the new Library as an editable table
  output$metaTable <- rhandsontable::renderRHandsontable({


        currentlyLoadedSamples <-  data.frame(Strain_ID = sampleIDs, `Property 1` = rep("",length(sampleIDs)))
            rhandsontable::rhandsontable(currentlyLoadedSamples,
                                          useTypes = FALSE,
                                          contextMenu = TRUE ) %>%
              hot_col("Strain_ID", readOnly = TRUE) %>%
              hot_context_menu(allowRowEdit = FALSE,
                               allowColEdit = TRUE)






    })





}


