#' A correctType Function
#'
#' 'correctType' reads data from .csv file into the correct data type.
#' This function processes the raw data into the correct data type.
#' @param raw Data in data frame format.
#' @param datatype The data type for each raw data column. Accepted data types are factor, numeric, integer,
#' date, ordered, and character.
#' @param cols If not all columns need correction, specify the column number that need correction.
#' @author Henry Nanji, Saisakul Chernbumroong
#' @keywords data type
#' @export
#' @return Returns the data set with the corrected data type.
#' @examples
#' library(FactoMineR)
#' data(wine)
#' correctType(wine, "factor", 1)

correctType = function(raw, datatype, cols = NULL)
{
  if(ncol(raw) != length(datatype) & is.null(cols))
    stop("The number of data column and data type do not match.
         Otherwise, specify the column number that need correction.")

  #Process data
  #Change data type checking using summary, levels

  preprocess <- function(x, nType)
  {
    nType = tolower(nType)
    x = switch (nType,
                     factor = as.factor(tolower(x)),
                     numeric = as.numeric(gsub(",", "", x)),
                     real = as.numeric(gsub(",", "", x)),
                     integer = as.integer(gsub(",", "", x)),
                     date = as.Date(x, "%d/%m/%Y"),
                     ordered = as.ordered(tolower(x)),
                     character = as.character(tolower(x)),
                     x)

  }



  #Process data to correct type

  if(is.data.frame(raw) != "data.frame")
  {
    raw = as.data.frame(raw)
  }


  processed = raw

  #If all columns need correction, generate column sequence.
  if(is.null(cols))
  {
    cols = seq(1, ncol(raw), 1)
  }

  for(i in cols)
  {
    tryCatch({

      processed[, i] = preprocess(raw[, i], as.character(datatype[i]))
    }, warning = function(e){suppressWarnings(raw)
      print(paste("Warning: There is a problem with variable",e, colnames(raw)[i]))},
    error = function(e){print(paste("There is a problem with variable",colnames(raw)[i]))})

  }


  return(processed)

}
