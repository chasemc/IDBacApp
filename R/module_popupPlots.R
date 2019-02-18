#' UI module for creating absolute panel popup
#'
#' @param id shiny namespace
#' @param name name of plot-type (eg PCA, PCoA, t-SNE, scatter)
#'
#' @return PCA UI
#' @export
#'
popupPlot_UI <- function(id, name){
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("openAbsPanel"), glue::glue("Open {name} plot")),
    uiOutput(ns("absPanel"))
  )
}


#' popupPlot_server
#' 
#' @param input shiny nput
#' @param output shiny output
#' @param session shiny session
#' @param dataFrame input dataframe, rows = samples, columns = variables
#' @param namedColors 
#' @param plotTitle 
#' @param extraUI 
#'
#' @return NA
#' @export
#'
popupPlot_server <- function(input,
                             output,
                             session,
                             dataFrame,
                             namedColors,
                             plotTitle){ 
  
  
  output$plot <- plotly::renderPlotly({
    req(nrow(dataFrame()) > 2,
        ncol(dataFrame()) > 2)
    
    
    if (is.null(namedColors())) {
      colorsToUse <- cbind.data.frame(fac = rep("#000000", nrow(dataFrame())), 
                                      dataFrame())
    } else {
      
      colorsToUse <- cbind.data.frame(fac = as.vector(namedColors()), 
                                      nam = (names(namedColors())))
      
      
      colorsToUse <- merge(dataFrame(),
                           colorsToUse, 
                           by = "nam")
    }
    
    plotly::plot_ly(data = colorsToUse,
                    x = ~Dim1,
                    y = ~Dim2,
                    z = ~Dim3,
                    type = "scatter3d",
                    mode = "markers",
                    marker = list(color = ~fac),
                    hoverinfo = 'text',
                    text = ~nam) %>% 
      plotly::layout(title = plotTitle)
  })
  
  observeEvent(input$closeAbsPanel, {
    output$absPanel <- renderUI({
      # Intentionally Blank
    })
  })  
  
  observeEvent(input$openAbsPanel,
               ignoreInit = T,
               ignoreNULL = T,
               {
                 
                 if (nrow(dataFrame()) < 2 || ncol(dataFrame()) < 3) {
                   output$absPanel <- renderUI(
                    glue::glue("Select more samples for {plotTitle}")
                   )
                 } else {
                 
                 
                 output$absPanel <- renderUI(
                   
                   shiny::fixedPanel(
                     class = "popup_Plots",
                     top = "20%",
                     bottom = "20%",
                     width = "60%",
                     draggable = F,
                     style = "z-index:1002;",
                     p(plotTitle),
                     absolutePanel(
                       top = "0%",
                       bottom = "95%",
                       right = "5%",
                       left = "95%",
                       fixed = F,
                       shiny::actionButton(session$ns("closeAbsPanel"),
                                           class = "closeX",
                                           label = "",
                                           icon = icon("far fa-window-close"))
                     ),
                     fluidRow(                     
                       plotly::plotlyOutput(session$ns("plot"),
                                            width = "100%", 
                                            height = "100%")
                     )
                   )
                 )
                 }
               })
}