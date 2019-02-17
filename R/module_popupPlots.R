
#' UI module for creating absolute panel popup
#'
#' @param id namespace
#' @param name name
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
#' @param input shiny
#' @param output shiny
#' @param session shiny
#' @param dataFrame input dataframe, rows=samples, cols =variables
#'
#' @return NA
#' @export
#'

popupPlot_server <- function(input,
                             output,
                             session,
                             dataFrame,
                             namedColors){ 
  
  
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
                    text = ~nam) 
    
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
                 output$absPanel <- renderUI(
                   
                   shiny::fixedPanel(
                     class = "popup_Plots",
                     top = "20%",
                     bottom = "20%",
                     # right =  "10%",
                     # left = "30%",
                     width = "60%",
                     draggable = F,
                     style = "z-index:1002;",
                     
                     
                     absolutePanel(
                       top = "0%",
                       bottom = "95%",
                       right = "5%",
                       left = "95%",
                       fixed =F,
                       shiny::actionButton(session$ns("closeAbsPanel"),
                                           class = "closeX",
                                           label = "",
                                           icon = icon("far fa-window-close"))
                     ),
                     absolutePanel(
                       top = "8%",
                       bottom = "1%",
                       right = "1%",
                       left = "1%",
                       fixed = F,
                       plotly::plotlyOutput(session$ns("plot"),
                                            width = "100%", 
                                            height = "100%")
                     )
                     
                     
                   )
                   
                   
                 )
                 
               })
}