#' A checkValidity Function
#'
#' 'checkValidity' checks the class of the different variables in the data frame and returns a list
#' of numeric and factor or character variables. It also returns basic summary statistics(mean, median, min, max) for numeric and categorical/character variables
#' It also returns the number of colunms and rows in the data frame.
#' @param dat Data in data frame format.
#' @param numeric Numeric data type.
#' @param cat Categorical or factor data type.
#' @author Henry Nanji, Saisakul Chernbumroong
#' @keywords datatype
#' @export
#' @return Returns a list of numeric and character(factor) variables, basic summary statistics and number of rows and columns in the data frame
#' @examples
#' library(FactoMineR)
#' data(wine)
#' checkValidity(wine, numeric=TRUE)


checkValidity <-function (dat, numeric = FALSE, cat = FALSE) {

  vars <-colnames(dat)

  print(paste("This data frame has ", nrow(dat), " rows and ", ncol(dat), " columns", sep = ""))


  if(cat == TRUE)
  {

    catVars <-vars[sapply(dat[,vars],class) %in% c('factor','character')] #loops through df and identifies which vars are categorical
    cat_summary <-summary(dat[catVars])
    print(paste("The following variabes are categorical or factor", sep = ""))
    return(list(catVars,cat_summary))
  }
  if (numeric == TRUE)
  {

  numeric_var <-vars[sapply(dat[,vars],class) %in% c('numeric')]
  numeric_summary <-summary(dat[numeric_var])
  print(paste("The following variabes are numeric", sep = ""))
  return(list(numeric_var, numeric_summary))


    }

}





