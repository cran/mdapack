% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missingAnalysis.R
\name{missingAnalysis}
\alias{missingAnalysis}
\title{A missingAnalysis Function}
\usage{
missingAnalysis(
  dat,
  plot = FALSE,
  miss_pattern = FALSE,
  complete_dat = FALSE,
  miss_cor_pattern = FALSE
)
}
\arguments{
\item{dat}{Data in  data frame format.}

\item{plot}{A plot of the missing data pattern.}

\item{miss_pattern}{A data frame with missing data pattern for each variable.}

\item{complete_dat}{A data frame with complete cases.}

\item{miss_cor_pattern}{Correlation between variables with missing values.}
}
\value{
Returns a complete data frame, a tabulation of missing data pattern and missing data plot for combination of variables
}
\description{
'missingAnalysis' function reveals the variables with missing values, the number of missing values for each variable, and in what combinations
It also produces a plot for visualizing pattern of missing values and returns a data frame showing correlation between pairs of variables that has  missing values
It also returns a data frame with complete cases
}
\examples{
library(VIM)
data(sleep)
missingAnalysis(sleep, complete_dat  = TRUE)
}
\author{
Henry  Nanji
}
\keyword{analysis}
\keyword{missing}
