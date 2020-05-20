#' A checkVariableNames Function
#'
#' 'checkVariableNames'checks if the given parameters' names and the columns' names of the data are the same.
#' @param raw Data in data.frame format.
#' @param parameterName Factor or characters of parameters' names.
#' @author Henry Nanji,Saisakul Chernbumroong
#' @keywords check variable name
#' @return Returns a list of the correct variable names
#' @export
#' @examples
#' data(iris)
#' checkVariableNames(iris, c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species"))

checkVariableNames = function(raw, parameterName)
{
  if(all(colnames(raw) == make.names(parameterName)))
  {
        message <- ("")
        results <-("The columns' names match the given parameters.")

  }
  else
  {
    message <-("Mismatch: This data frame has the following columns:")
   results <-paste(colnames(raw), sep = "")
  }
  return(list(message, results))

}



