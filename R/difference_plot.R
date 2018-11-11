#' Create a Difference Plot
#'
#' This function creates a Difference plot. It takes two numeric vectors x - the measurand and 
#' y - the reference as inputs.
#' It also takes an optional input to specify if the plotly package should
#' be used or not for plotting.
#' It plots the reference variable y on the x-axis
#' and the ratio of differences between measurand and reference to
#' the reference on the y-axis
#' @param x First input numeric vector
#' @param y Second input numeric vector
#' @param plot.type Optional argument. Plots using base R graphics if not specified. Options include 'plotly'.
#' @return A plot object
#' @export
difference_plot <- function(measurand, reference, plot.type){

  # Sanity checks
  # Check if measurand and reference are both numeric vectors
  if (!is.vector(measurand, mode = "numeric") | !is.vector(reference, mode = "numeric")){
    stop("Both inputs should be numeric vectors")
  }
  
  # Check if x and y are of the same length
  if (length(measurand) != length(reference)){
    stop("Vectors should be of the same length.")
  }
  
  # Populate difference vector
  abs.diff <- (measurand - reference)/reference

  # Create plot
  # If plot type not specified, use base R graphics
  if (missing(plot.type)){
    p <- plot(x = reference, y = abs.diff, pch = 16, 
              col = "blue", xlab = "Reference", 
              ylab = "(Measurand - Reference) / Reference")
    title("Difference Plot")
  }
  # Else use input plot type
  else if(plot.type == "plotly"){
    p <- plotly::plot_ly(x = reference, y = abs.diff, type = 'scatter', mode = 'markers') %>%
      plotly::layout(title = 'Difference Plot',
             xaxis = list(title = "Reference", showgrid = TRUE, zeroline = FALSE),
             yaxis = list(title = "(Measurand - Reference) / Reference", showgrid = TRUE, zeroline = FALSE))
  }
  return(p)
}