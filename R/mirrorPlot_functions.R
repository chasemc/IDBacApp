#' Create zoomed mirror plot
#'
#' @param mirrorPlotEnv data for plot
#' @param nameOne sample id of top spectrunm
#' @param nameTwo sample id of bottom spectrum
#' @param ranges2 brushed ranges
#'
#' @return
#'
mirrorPlotZoom <- function(mirrorPlotEnv,
                           nameOne,
                           nameTwo,
                           ranges2){
  
  graphics::plot(x = mirrorPlotEnv$spectrumSampleOne@mass,
                 y = mirrorPlotEnv$spectrumSampleOne@intensity,
                 xlim = ranges2$x, ylim = ranges2$y,
                 type = "l",
                 col = adjustcolor("Black", alpha = 0.3),
                 xlab = "m/z",
                 ylab = "Intensity")
  graphics::lines(x = mirrorPlotEnv$spectrumSampleTwo@mass,
                  y = -mirrorPlotEnv$spectrumSampleTwo@intensity)
  rect(xleft = mirrorPlotEnv$peaksSampleOne@mass - 0.5,
       ybottom = 0,
       xright = mirrorPlotEnv$peaksSampleOne@mass + 0.5,
       ytop = ((mirrorPlotEnv$peaksSampleOne@intensity) * max(mirrorPlotEnv$spectrumSampleOne@intensity) / max(mirrorPlotEnv$peaksSampleOne@intensity)),
       border = mirrorPlotEnv$SampleOneColors)
  rect(xleft = mirrorPlotEnv$peaksSampleTwo@mass - 0.5,
       ybottom = 0,
       xright = mirrorPlotEnv$peaksSampleTwo@mass + 0.5,
       ytop = -((mirrorPlotEnv$peaksSampleTwo@intensity) * max(mirrorPlotEnv$spectrumSampleTwo@intensity) / max(mirrorPlotEnv$peaksSampleTwo@intensity)),
       border = rep("grey", times = length(mirrorPlotEnv$peaksSampleTwo@intensity)))
  legend(max(ranges2$x) * .85,
         max(ranges2$y) * .7, 
         legend = c(paste0("Top: ", nameOne),
                    paste0("Bottom: ", nameTwo)),
         col = c("black", "black"),
         lty = 1:1,
         cex = 1)
}








#' Create mirror plot
#'
#' @param mirrorPlotEnv data for plot
#'
#' @return NA
#' @export
#'
mirrorPlot <- function(mirrorPlotEnv){
  
  
  #Create peak plots and color each peak according to whether it occurs in the other spectrum
  graphics::plot(x = mirrorPlotEnv$spectrumSampleOne@mass,
                 y = mirrorPlotEnv$spectrumSampleOne@intensity,
                 ylim = c(-max(mirrorPlotEnv$spectrumSampleTwo@intensity),
                          max(mirrorPlotEnv$spectrumSampleOne@intensity)),
                 type = "l",
                 col = adjustcolor("Black", alpha = 0.3),
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