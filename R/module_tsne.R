



#' UI module for creating absolute panel popup
#'
#' @param id shiny namespace
#'
#' @return PCA UI
#' @export
#'
popupPlotTsne_UI <- function(id){
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("openAbsPanel"), "Open t-SNE plot"),
    uiOutput(ns("absPanel"))
  )
}


#' popupPlot_server
#' 
#' @param input shiny nput
#' @param output shiny output
#' @param session shiny session
#' @param namedColors named colors to have same colors as dend
#' @param data data
#' @param plotTitle title of plot
#'
#' @return NA
#' @export
#'
popupPlotTsne_server <- function(input,
                                 output,
                                 session,
                                 data,
                                 namedColors,
                                 plotTitle = "t-SNE"){ 
  
  
  
  
  
  dataFrame <- reactive({
    
    validate(need(nrow(data()) > 1, "t-SNE requires more samples"))
    
    validate(need(input$tsnePerplexity > 0, "Perplexity must be greater than 0"))
    validate(need(input$tsnePerplexity < 101, "Perplexity must be less than 101"))
    
    validate(need(input$tsneTheta > 0, "Theta must be greater than 0"))
    validate(need(input$tsneTheta < 1, "Theta must be less than 101"))
    
    validate(need(input$tsneIterations > 0, "Iterations must be greater than 0"))
    validate(need(input$tsneIterations < 10000, "Iterations must be less than 101"))
    
    #IDBacApp::tsneCalculation(dataMatrix = dataframe(),
    IDBacApp::tsneCalculation(dataMatrix = data(),
                              perplexity = input$tsnePerplexity,
                              theta = input$tsneTheta,
                              iterations = input$tsneIterations)
    
  })
  
  
  
  
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
                       tagList(
                         shiny::numericInput(inputId = session$ns("tsnePerplexity"),
                                             label = "Perplexity",
                                             value = 10,
                                             min = 1,
                                             max = 100,
                                             step = 1,
                                             width = NULL),
                         shiny::numericInput(inputId = session$ns("tsneTheta"),
                                             label = "Theta",
                                             value = .5,
                                             min = 0,
                                             max = 1,
                                             step = 0.1,
                                             width = NULL),
                         shiny::numericInput(inputId = session$ns("tsneIterations"),
                                             label = "Iterations",
                                             value = 1000,
                                             min = 1,
                                             max = 10000,
                                             step = 1,
                                             width = NULL)
                         
                       )
                     ),
                     
                     fluidRow(                     
                       plotly::plotlyOutput(session$ns("plot"),
                                            width = "100%", 
                                            height = "100%")
                     )
                   )
                 )
                 
               })
}