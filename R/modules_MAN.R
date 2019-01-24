



manPageProtDend <- function(input,
                            output,
                            session,
                            dendroReact,
                            colorByLines,
                            cutHeightLines,
                            colorByLabels,
                            cutHeightLabels,
                            plotHeight = 500,
                            plotWidth = 20){
  
  output$hierOut <- renderPlot({
    
    par(mar = c(5, 5, 5, plotWidth))
    
    plot(dendroReact(), horiz = TRUE)
    
    if(!is.null(colorByLines())){
      if(colorByLines() == "height"){
        abline(v= cutHeightLines(), lty = 2)
        
      }
    }
    
    if(!is.null(colorByLabels())){
      if(colorByLabels() == "height"){
        abline(v= cutHeightLines(), lty = 2)
      }
    }
    
  }, height=plotHeight)
  
  output$info <- renderPrint({ brushedPoints(input$plot_brush) })

}


manPageProtDendUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
  plotOutput(ns("hierOut"),
             brush = ns("plot_brush")),
  verbatimTextOutput("info")
  )
  
  
  
}
