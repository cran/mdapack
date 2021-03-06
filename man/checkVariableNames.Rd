% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkVariableNames.R
\name{checkVariableNames}
\alias{checkVariableNames}
\title{A checkVariableNames Function}
\usage{
checkVariableNames(raw, parameterName)
}
\arguments{
\item{raw}{Data in data.frame format.}

\item{parameterName}{Factor or characters of parameters' names.}
}
\value{
Returns a list of the correct variable names
}
\description{
'checkVariableNames'checks if the given parameters' names and the columns' names of the data are the same.
}
\examples{
data(iris)
checkVariableNames(iris, c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species"))
}
\author{
Henry Nanji,Saisakul Chernbumroong
}
\keyword{check}
\keyword{name}
\keyword{variable}
