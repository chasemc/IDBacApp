
sampleChooserUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    p("sdaa"),
    shiny::uiOutput(ns("chooseSamples"))
    
  )
}

sampleChooser <- function(input,
                          output,
                          session,
                          pool,
                          whetherProtein = FALSE,
                          allSamples = FALSE){

  
  
amcd <- reactive({
  
  IDBacApp::availableSampleNames(pool = pool,
                                 whetherProtein = whetherProtein,
                                 allSamples = allSamples)
})  

    output$chooseSamples <- renderUI({
      ns <- session$ns
 
    IDBacApp::chooserInput(ns("addSampleChooser"),
                           "Available samples",
                           "Selected samples",
                           amcd(),
                           c(),
                           size = 10,
                           multiple = TRUE
    )
  })
  
  

  return(input$addSampleChooser)
}