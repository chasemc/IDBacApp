graphics::plot(x = mirrorPlotEnv$spectrumSampleOne@mass,
               y = mirrorPlotEnv$spectrumSampleOne@intensity,
               ylim = c(-max(mirrorPlotEnv$spectrumSampleTwo@intensity),
                        max(mirrorPlotEnv$spectrumSampleOne@intensity)),
               type = "l",
               col = adjustcolor("Black", alpha.f = 0.3),
               xlab = "m/z",
               ylab = "Intensity")


ggP <- cbind.data.frame(x = mirrorPlotEnv$spectrumSampleOne@mass,
                 y = mirrorPlotEnv$spectrumSampleOne@intensity)




mLeft <- mirrorPlotEnv$peaksSampleOne@mass - 1
mRight <- mirrorPlotEnv$peaksSampleOne@mass + 1
intTop <- (mirrorPlotEnv$peaksSampleOne@intensity) * max(mirrorPlotEnv$spectrumSampleOne@intensity) / max(mirrorPlotEnv$peaksSampleOne@intensity)

lp <- lapply(seq_along(mirrorPlotEnv$SampleOneColors), 
       function(x){
         
         list(type = "rect",
          fillcolor = mirrorPlotEnv$SampleOneColors[[x]],
          line = list(color = "grey"),
          opacity = 0.3,
          x0 = mLeft[[x]],
          x1 = mRight[[x]], 
          xref = "x",
          y0 = 0, 
          y1 = intTop[[x]], 
          yref = "y")
         
         
       })






plotly::plot_ly(data = ggP,
                x = ~x,
                y = ~y,
                type = "scatter",
                mode = "line") %>% 
  plotly::add_lines(data = ggP,
                    x = ~x,
                    y = ~-y) %>% 
  plotly::layout(shapes = lp)


