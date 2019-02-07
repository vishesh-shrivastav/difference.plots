#' Create a Bland Altman Plot
#'
#' This function creates a Bland Altman plot. It takes two numeric vectors x and y as inputs.
#' It also takes an optional input to specify if the plotly package should
#' be used or not for plotting.
#' @param x First input numeric vector
#' @param y Second input numeric vector
#' @param plot.type Optional argument. Plots using base R graphics if not specified. Options include 'plotly'.
#' @return A plot object
#' @export
bland_altman <- function(x, y, plot.type){
  
  # Sanity checks
  # Check if x and y are both numeric vectors
  if (!is.vector(x, mode = "numeric") | !is.vector(y, mode = "numeric")){
    stop("Both inputs should be numeric vectors")
  }
  
  # Check if x and y are of the same length
  if (length(x) != length(y)){
    stop("Vectors should be of the same length.")
  }
  
  # Populate differences
  means <- (x + y)/2
  diffs <- (x - y)
  
  # Populate vectors for mean of differences and standard deviation of differences 
  mean.of.differences <- mean(diffs, na.rm = TRUE)
  sd.of.differences <- sd(diffs, na.rm = TRUE)
    
  # Create plot
  # If plot type not specified, use base R graphics
  if (missing(plot.type)){
    p <- plot(x = means, y = diffs, pch = 16, 
              col = "blue", xlab = "Means", 
              ylab = "Differences")
    title("Bland Altman Plot")
    # Add Line for mean of differences
    abline(h = mean.of.differences, , col = "red")
    # Dotted line for upper CI
    abline(h = mean.of.differences + 2 * sd.of.differences, col = "red", lty = "dashed")
    # Dotted line for lower CI
    abline(h = mean.of.differences - 2 * sd.of.differences, col = "red", lty = "dashed")
  }
  # Else use input plot type
  else if(plot.type == "plotly"){
    p <- plotly::plot_ly(x = means, y = diffs, type = 'scatter', mode = 'markers')
    
    p <- plotly::layout(p, title = 'Bland Altman Plot',
                     xaxis = list(title = "Means", showgrid = TRUE, zeroline = FALSE),
                     yaxis = list(title = "Differences", showgrid = TRUE, zeroline = FALSE),
                     shapes = (list(
                       # Add Line for mean of differences
                       list(type = 'line', xref = 'paper',
                            x0 = 0, x1 = 1,
                            y0 = mean.of.differences,
                            y1 = mean.of.differences,
                            line = list(width=1)),
                       # Dotted line for upper CI
                       list(type = 'line', xref = 'paper',
                            x0 = 0, x1 = 1,
                            y0 = mean.of.differences + 2 * sd.of.differences,
                            y1 = mean.of.differences + 2 * sd.of.differences,
                            line = list(width = 1, dash = 'dash')),
                       # Dotted line for lower CI
                       list(type = 'line', xref = 'paper',
                            x0 = 0, x1 = 1,
                            y0 = mean.of.differences - 2 * sd.of.differences,
                            y1 = mean.of.differences - 2 * sd.of.differences,
                            line = list(width = 1, dash = 'dash'))
                     ))
      )
  }
  # Else stop and raise error
  else{
    stop("Incorrect plot.type specified")
  }
  
  return(p)
}