
#' A plotBoxplotGroup Function
#'
#' 'plotBoxplotGroup' function plots boxplots by group.
#' @param dat Data in frame format.
#' @param x A grouping variable.
#' @param ... Other parameters.
#' @author Henry Nanji
#' @keywords box plot
#' @export
#' @return Returns a box plot for each grouping variable
#' @examples
#' data(iris)
#' plotBoxplotGroup(iris, 'Species')
plotBoxplotGroup = function(dat, x,...) {

  # get numeric columns
  numeric_names <- names(dat[, sapply(dat, is.numeric)])

  if(requireNamespace("ggplot2", quietly = TRUE)) {
    # loop through numeric columns

    for (numeric_col in numeric_names) {
      # do the plot with ggplot

      plot <- ggplot2::ggplot(dat, ggplot2::aes_string(x = x, y = numeric_col, color = x)) +
        ggplot2::geom_boxplot()

      return(plot)
    }


  }


}
