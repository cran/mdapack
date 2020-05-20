#' Outlier detection function
#'
#' 'outlier_detection'visually detect and highlights outliers in a univariate continuous variable.
#' The function fetches the values of data points that lie beyond the extremes of the whiskers(observations that lie outside of 1.5 * IQR.
#' @param dat A data frame for data values.
#' @param ... Other arguments.
#' @author Henry  Nanji
#' @keywords outlier
#' @return Returns a box plot showing the outliers for each variable.
#' @export
#' @examples
#' library(FactoMineR)
#' data(wine)
#' outlier_detection(wine)

outlier_detection <-function(dat,...){


  # get numeric columns
  numeric_names <- names(dat[, sapply(dat, is.numeric)])

  if(requireNamespace("ggplot2", quietly = TRUE)) {
    # loop through numeric columns

    for (numeric_col in numeric_names) {

     # Loop over loop.vector

    outlier_values <- graphics::boxplot(dat[,numeric_col])$out

   graphics::boxplot(dat[,numeric_col], main = paste(numeric_col), boxwex=0.1)

     plot <-graphics::mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

    }
  }
    return(plot)

}





