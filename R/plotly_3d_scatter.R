
#' Create 3d scatter plotly plot
#'
#' @param data data frame with data and colors and sample names
#' @param plotTitle plot title
#'
#' @return plotly plot
#' @export
#'
plotly_3d_scatter <- function(data,
                              plotTitle){
  plotly::plot_ly(data = data,
                  x = ~Dim1,
                  y = ~Dim2,
                  z = ~Dim3,
                  type = "scatter3d",
                  mode = "markers",
                  marker = list(color = ~fac),
                  hoverinfo = 'text',
                  text = ~nam) %>% 
    plotly::layout(title = plotTitle)
}