#' A missingAnalysis Function
#'
#' 'missingAnalysis' function reveals the variables with missing values, the number of missing values for each variable, and in what combinations
#' It also produces a plot for visualizing pattern of missing values and returns a data frame showing correlation between pairs of variables that has  missing values
#' It also returns a data frame with complete cases
#' @param dat Data in  data frame format.
#' @param miss_pattern A data frame with missing data pattern for each variable.
#' @param plot A plot of the missing data pattern.
#' @param complete_dat A data frame with complete cases.
#' @param miss_cor_pattern Correlation between variables with missing values.
#' @author Henry  Nanji
#' @keywords missing analysis
#' @export
#' @return Returns a complete data frame, a tabulation of missing data pattern and missing data plot for combination of variables
#' @examples
#' library(VIM)
#' data(sleep)

#' missingAnalysis(sleep, complete_dat  = TRUE)

missingAnalysis = function(dat, plot = FALSE, miss_pattern=FALSE, complete_dat = FALSE, miss_cor_pattern = FALSE)
{

  if(requireNamespace("VIM", quietly = TRUE))

    if(requireNamespace("mice", quietly = TRUE))



      if(complete_dat == TRUE) {
        # returns a data frame with complete observations
        complete_dat <-data.frame(dat[stats::complete.cases(dat),])
        return(complete_dat)

      }


  #
  if(miss_pattern==TRUE) {

    #tabulation of missing data pattern for each variable
    miss_pattern <-data.frame(mice::md.pattern(dat))
    return(miss_pattern)

  }


  if(plot ==TRUE) {

    # Plot the number of missing values for each variable and for each combination of variable
    plot <-VIM::aggr(dat, prop=FALSE, numbers=TRUE)
    return(plot)

  }

  if(miss_cor_pattern == TRUE) {

    # correlations between variables

    x <- as.data.frame(abs(is.na(dat)))

    # extracting the variables that have some missing values

    y <- x[which(sapply(x, stats::sd) > 0)]

    cor_results <-stats::cor(y)

    return(cor_results)

  }

}
