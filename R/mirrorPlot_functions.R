
#' Create mirror plot
#'
#' @param mirrorPlotEnv data for plot
#'
#' @return NA
#' @export
#'
mirrorPlot <- function(mirrorPlotEnv){
  
  
  
  top <- cbind.data.frame(x = mirrorPlotEnv$spectrumSampleOne@mass,
                          y = mirrorPlotEnv$spectrumSampleOne@intensity)
  
  
  
  
  mLeft <- mirrorPlotEnv$peaksSampleOne@mass - 5
  mRight <- mirrorPlotEnv$peaksSampleOne@mass + 5
  intTop <- (mirrorPlotEnv$peaksSampleOne@intensity) * max(mirrorPlotEnv$spectrumSampleOne@intensity) / max(mirrorPlotEnv$peaksSampleOne@intensity)
  
  lp <- lapply(seq_along(mirrorPlotEnv$SampleOneColors), 
               function(x){
                 
                 list(type = "rect",
                      fillcolor = mirrorPlotEnv$SampleOneColors[[x]],
                      line = list(color = mirrorPlotEnv$SampleOneColors[[x]]),
                      opacity = .5,
                      x0 = mLeft[[x]],
                      x1 = mRight[[x]], 
                      xref = "x",
                      y0 = 0, 
                      y1 = intTop[[x]], 
                      yref = "y")
                 
                 
               })
  bottom <- cbind.data.frame(x = mirrorPlotEnv$spectrumSampleTwo@mass,
                          y = mirrorPlotEnv$spectrumSampleTwo@intensity)
  
  
  
  
  mLeft <- mirrorPlotEnv$peaksSampleTwo@mass - 10
  mRight <- mirrorPlotEnv$peaksSampleTwo@mass + 10
  intTop <- -((mirrorPlotEnv$peaksSampleTwo@intensity) * max(mirrorPlotEnv$spectrumSampleTwo@intensity) / max(mirrorPlotEnv$peaksSampleTwo@intensity))
  
  lp2 <- lapply(seq_along(mirrorPlotEnv$SampleTwoSNR), 
               function(x){
                 
                 list(type = "rect",
                      fillcolor = "grey",
                      line = list(color = "grey"),
                      opacity = 0.5,
                      x0 = mLeft[[x]],
                      x1 = mRight[[x]], 
                      xref = "x",
                      y0 = 0, 
                      y1 = intTop[[x]], 
                      yref = "y")
                 
                 
               })
  
  
  plotly::plot_ly(data = top,
                  x = ~x,
                  y = ~y,
                  type = "scatter",
                  mode = "line",
                  text  = paste("m/z:", round(top$x, 3),
                                "<br> Intensity:", round(top$y, 1),
                                    "<br> Sample:",
                                    mirrorPlotEnv$peaksSampleOne@metaData$Strain),
                  hoverinfo = 'text',
                  line = list(color = "#000000"),
                  name = mirrorPlotEnv$peaksSampleOne@metaData$Strain) %>% 
    plotly::add_lines(data = bottom,
                      x = ~x,
                      y = ~-y,
                      line = list(color = "#E69F00"),
                      name = mirrorPlotEnv$peaksSampleTwo@metaData$Strain,
                      text  = paste("m/z:", round(bottom$x, 3),
                                    "<br> Intensity:", round(bottom$y, 1),
                                    "<br> Sample:",
                                    mirrorPlotEnv$peaksSampleTwo@metaData$Strain),
                      hoverinfo = 'text') %>% 
    plotly::layout(shapes = c(lp, lp2),
                   showlegend = FALSE) 
                   
  
  
  
  
  
}







#' Create mirror plot in base R
#'
#' @param mirrorPlotEnv data for plot
#'
#' @return NA
#' @export
#'
baserMirrorPlot <- function(mirrorPlotEnv){
  
  
  #Create peak plots and color each peak according to whether it occurs in the other spectrum
  graphics::plot(x = mirrorPlotEnv$spectrumSampleOne@mass,
                 y = mirrorPlotEnv$spectrumSampleOne@intensity,
                 ylim = c(-max(mirrorPlotEnv$spectrumSampleTwo@intensity),
                          max(mirrorPlotEnv$spectrumSampleOne@intensity)),
                 type = "l",
                 col = adjustcolor("Black", alpha.f = 0.3),
                 xlab = "m/z",
                 ylab = "Intensity")
  graphics::lines(x = mirrorPlotEnv$spectrumSampleTwo@mass,
                  y = -mirrorPlotEnv$spectrumSampleTwo@intensity)
  graphics::rect(xleft = mirrorPlotEnv$peaksSampleOne@mass - 0.5,
                 ybottom = 0,
                 xright = mirrorPlotEnv$peaksSampleOne@mass + 0.5,
                 ytop = ((mirrorPlotEnv$peaksSampleOne@intensity) * max(mirrorPlotEnv$spectrumSampleOne@intensity) / max(mirrorPlotEnv$peaksSampleOne@intensity)),
                 border = mirrorPlotEnv$SampleOneColors)
  graphics::rect(xleft = mirrorPlotEnv$peaksSampleTwo@mass - 0.5,
                 ybottom = 0,
                 xright = mirrorPlotEnv$peaksSampleTwo@mass + 0.5,
                 ytop = -((mirrorPlotEnv$peaksSampleTwo@intensity) * max(mirrorPlotEnv$spectrumSampleTwo@intensity) / max(mirrorPlotEnv$peaksSampleTwo@intensity)),
                 border = rep("grey", times = length(mirrorPlotEnv$peaksSampleTwo@intensity)))
}