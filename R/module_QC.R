


#' QC module variance and smoothing
#'
#' @param id namespace
#'
#' @return modulke UI
#' @export
#'
#' @examples
qc_module_variance_smooth_UI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("vs"),
                label = "Variance Stabilization:",
                choices = c("sqrt",
                            "log",
                            "log2",
                            "log10"),
                selected = "sqrt"),
    selectInput(inputId = ns("smoothingAlgo"),
                label = "Smoothing:",
                choices = c("Savitzky-Golay" = "SavitzkyGolay",
                            "Moving-Average" = "MovingAverage"),
                selected = "Savitzky-Golay"),
    sliderInput(inputId = ns("smoothingHW"),
                label = "halfWindowSize:",
                min = 1,
                max = 100,
                value = 10)
  )
  
}

#' QC module baseline
#' @param id namespace
#'
#' @return modulke UI
#' @export
#'
#' @examples
qc_module_baseline_UI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("baseline"), 
                label = "Baseline Correction:",
                choices = c("SNIP" = "SNIP", 
                            "TopHat" = "TopHat",
                            "ConvexHull" = "ConvexHull",
                            "Median" = "median"),
                selected = "SNIP"),
    conditionalPanel(condition ='inputbaseline != "ConvexHull"', ns = ns,
                     sliderInput(inputId = ns("baselineHW"),
                                 label = "halfWindowSize:",
                                 min = 1,
                                 max = 500,
                                 value = 100)),
    tags$hr(),
    
    checkboxInput(inputId = ns("bcUS"),
                  label = "Show estimated baseline",
                  value = TRUE),
    checkboxInput(inputId = ns("bcBC"),
                  label = "Show baseline corrected spectrum",
                  value = TRUE)
  )
  
}

#' QC module detecting peaks
#' @param id namespace
#'
#' @return modulke UI
#' @export
#'
#' @examples
qc_module_detection_UI <- function(id){
  ns <- NS(id)
  tagList(
    sliderInput(inputId = ns("detecthalfWindow"),
                label = "halfWindowSize:",
                min = 1,
                max = 500,
                value = 20),
    sliderInput(inputId = ns("snr"), 
                label = "SNR (signal-to-noise-ration):",
                min = 1,
                max = 100,
                value = 2),
    selectInput(inputId = ns("noise"), 
                label = "Noise Estimator:",
                choices = c("MAD",
                            "Friedman's SuperSmoother" = "SuperSmoother"),
                selected = "MAD"),
    tags$hr(),
    p(strong("Label Peaks")),
    sliderInput(inputId = ns("topN"),
                label = "Label Top N Peaks:",
                min = 0,
                max = 100,
                value = 5),
    checkboxInput(inputId = ns("rotatePeakLables"),
                  label = "Rotate Peak Labels",
                  value = TRUE)
    
  )
}











#' QC module sliders
#' @param id namespace
#'
#' @return modulke UI
#' @export
#'
#' @examples
qc_module_sliders_UI <- function(id){
  ns <- NS(id)
  
  wellPanel(
    p(strong("Zoom:")),
    uiOutput(ns("xlimSlider")),
    sliderInput(inputId = ns("ylim"),
                label = "Intensity Range:",
                min = 0,
                max = 100, 
                value = c(0,100),
                ticks = TRUE),
    p(strong("Plot:")),
    uiOutput(ns("selectSpectra"))
  )
  
}





#' QC main module
#' @param id namespace
#'
#' @return modulke UI
#' @export
#'
#' @examples
qc_module_main_UI <- function(id){
  ns <- NS(id)
  fluidPage(
    column(width = 7,
           fluidRow(
             IDBacApp::bsCollapse(id = ns("variance"),
                                  IDBacApp::bsCollapsePanel(h4("Variance and Smoothing"), 
                                                            column(width = 4,
                                                                   IDBacApp::qc_module_variance_smooth_UI(id)
                                                            ), 
                                                            column(width = 8,
                                                                   plotOutput(ns("plotSmooth"))
                                                            )
                                                            
                                  )),
             IDBacApp::bsCollapse(id = ns("baseline"),
                                  
                                  IDBacApp::bsCollapsePanel(h4("Baseline Correction"), 
                                                            column(width = 4,
                                                                   IDBacApp::qc_module_baseline_UI(id)
                                                            ),
                                                            column(width = 8,
                                                                   plotOutput(ns("baselinePlots"))
                                                            )
                                                            
                                                            
                                  )),
             IDBacApp::bsCollapse(id = ns("detect"),
                                  IDBacApp::bsCollapsePanel(h4("Peak Detection"), 
                                                            column(width = 4,
                                                                   IDBacApp::qc_module_detection_UI(id)
                                                            ),
                                                            column(width = 8,
                                                                   plotOutput(ns("peakPlots"))
                                                            )
                                                            
                                  ))
           )
    ), 
    column(width = 4, offset = 1,
           fluidRow(IDBacApp::qc_module_sliders_UI(id)),
           fluidRow(
             div(align = "center",
             h4("Original Spectrum"),
             plotOutput(ns("plotRaw"))
             )
           )
    )
    
    
    
    
  )
  
}



qc_module_server <- function(input,
                             output,
                             session,
                             mzFilePaths){
  
  
  output$tempMzFiles <- renderUI({
    
    list.files(tempMZ, tempMZ)
    
    
  })
  
  currentSpectra <- reactive({
    
    
    lapply(spectraImport, function(x) MALDIquant::createMassSpectrum(mass = x[ , 1],
                                                                                      intensity = x[ , 2],
                                                                                      metaData = list(File = rawDataFilePath,
                                                                                                      Strain = sampleID)))    
  })
  
  
  output$xlimSlider <- renderUI({
    
    sliderInput(inputId = session$ns("xlim"),
                label = "Mass Range:",
                min = floor(min(currentSpectra()@mass)),
                max = ceiling(max(currentSpectra()@mass)), 
                value = c(floor(min(currentSpectra()@mass)), ceiling(max(currentSpectra()@mass))),
                ticks = TRUE)
  })
  
  
  vsSpectra <- reactive({
    if (is.null(input$vs)) {
      method <- "sqrt"
    } else {
      method <- input$vs
    }
    return(MALDIquant::transformIntensity(currentSpectra(), method = method))
  })
  
  smoothedSpectra <- reactive({
    if (is.null(input$smoothingAlgo)) {
      method <- "SavitzkyGolay"
      hws <- 10
    } else {
      method <- input$smoothingAlgo
      hws <- input$smoothingHW
    }
    return(MALDIquant::smoothIntensity(vsSpectra(),
                                       method = method, 
                                       halfWindowSize = hws))
  })
  
  baselineCorrectedSpectra <- reactive({
    if (is.null(input$baseline)) {
      method <- "SNIP"
      hws <- 100
    } else {
      method <- input$baseline
      hws <- input$baselineHW
    }
    
    y <- smoothedSpectra()
    bl <- MALDIquant::estimateBaseline(y, 
                                       method = method,
                                       hws)
    MALDIquant::intensity(y) <- MALDIquant::intensity(y) - bl[ , 2]
    return(y)
    
  })
  
  detectedPeaks <- reactive({
    return(MALDIquant::detectPeaks(baselineCorrectedSpectra(),
                                   method = input$noise,
                                   halfWindowSize = input$detectHW, 
                                   SNR = input$snr))
  })
  
  
  
  output$plotRaw <- renderPlot({
    MALDIquant::plot(currentSpectra(),
                     xlim = c(input$xlim[[1]], input$xlim[[2]]),
                     ylim = c(max(currentSpectra()@intensity) * (input$ylim[[1]]  / 100), 
                              max(currentSpectra()@intensity) * (input$ylim[[2]]  / 100))
    )
  })
  
  
  
  output$plotSmooth <- renderPlot({
    
    
    MALDIquant::plot(vsSpectra(),
                     xlim = input$xlim,
                     ylim = c(max(vsSpectra()@intensity) * (input$ylim[[1]]  / 100), 
                              max(vsSpectra()@intensity) * (input$ylim[[2]]  / 100))
    )
    MALDIquant::lines(MALDIquant::smoothIntensity(vsSpectra(), 
                                                  method = input$smoothingAlgo,
                                                  halfWindowSize = input$smoothingHW), 
                      col = "red")
    
    
  })
  
  output$baselinePlots <- renderPlot({
    bl <- MALDIquant::estimateBaseline(smoothedSpectra(), 
                                       method = input$baseline,
                                       input$baselineHW)
    
    MALDIquant::plot(smoothedSpectra(),
                     xlim = input$xlim,
                     ylim = c(0, 
                              max(smoothedSpectra()@intensity) * (input$ylim[[2]]  / 100))
    )
    
    
    if (input$bcUS) {
      MALDIquant::lines(bl, 
                        col = "red",
                        lwd = 2)
    }
    if (input$bcBC) {
      MALDIquant::lines(baselineCorrectedSpectra(), col = 4)
      
      
    }
    
  })
  
  output$peakPlots <- renderPlot({
    MALDIquant::plot(baselineCorrectedSpectra(),
                     xlim = input$xlim,
                     ylim = c(max(baselineCorrectedSpectra()@intensity) * (input$ylim[[1]]  / 100), 
                              max(baselineCorrectedSpectra()@intensity) * (input$ylim[[2]]  / 100))
    )
    
    n <- MALDIquant::estimateNoise(baselineCorrectedSpectra(),
                                   method = input$noise)
    lines(n[, 1],
          input$snr*n[ , 2],
          col = 2,
          lwd = 2)
    p <- MALDIquant::detectPeaks(baselineCorrectedSpectra(), 
                                 method = input$noise,
                                 halfWindowSize = input$detecthalfWindow,
                                 SNR = input$snr)
    MALDIquant::points(p,
                       col = 4,
                       pch = 4,
                       lwd = 2)
    
    if (input$topN) {
      top <- sort(MALDIquant::intensity(p), 
                  decreasing = TRUE,
                  index.return = TRUE,
                  method = "quick")$ix[1:input$topN]
      if (input$rotatePeakLables) {
        srt <- 90
        adj <- c(-0.1, 0.5)
      } else {
        srt <- 0
        adj <- c(0.5, 0)
      }
      MALDIquant::labelPeaks(p[top], 
                             srt = srt,
                             adj = adj)
    }
    
  })
  
}

