#' Create a Mountain Plot
#'
#' This function creates a mountain plot. It takes two numeric vectors x and y as inputs.
#' It also takes an optional input to specify if the plotly package should
#' be used or not for plotting.
#' @param x First input numeric vector
#' @param y Second input numeric vector
#' @param plot.type Optional argument. The plot type to use. Plots using base R graphics if not specified. Options include 'plotly'.
#' @return A plot object
#' @export
mountain_plot <- function(x, y, plot.type){
  
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
  diffs <- abs(x - y)
  # Sort these differences in ascending order
  sorted.diffs <- sort(diffs)
  # Denominator = number of observations + 1
  denom <- length(sorted.diffs) + 1 
  
  # Populate vector of CDFs
  fun1 <- function(x){
    x / denom
  }
  cdfs <- sapply(seq_along(sorted.diffs), fun1)
  
  # Function to populate adjusted CDFs
  fun2 <-  function(x){
    if (x > 0.5){
      return(1 - x)
    } 
    else{
      return(x)
    }
  }
  # Populate adjusted CDFs
  adjusted.cdfs <- sapply(cdfs, fun2)
  
  # Create plot
  # If plot type not specified, use base R graphics
  if (missing(plot.type)){
    p <- plot(x = sorted.diffs, y = adjusted.cdfs, type = "o", col="blue", xlab = "Difference", 
              ylab = "Adjusted Cumulative Probability")
    title("Mountain Plot")
  }
  # Else use input plot type
  else if(plot.type == "plotly"){
    p <- plotly::plot_ly(x = sorted.diffs, y = adjusted.cdfs, type = 'scatter', mode = 'lines+markers') %>%
      plotly::layout(title = 'Mountain Plot',
                     xaxis = list(title = "Difference", showgrid = TRUE, zeroline = FALSE),
                     yaxis = list(title = "Adjusted Cumulative Probability", showgrid = TRUE, zeroline = FALSE))
  }
  # Else stop and raise error
  else{
    stop("Incorrect plot.type specified")
  }
  
  return(p)
}