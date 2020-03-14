#' A univariateAnalysis Function
#'
#' 'univariateAnalysis' produces univariate plots such as histogram, box plot and  q-q plot. It also calculates
#' the Shapiro-Wilk statistic for numeric data.
#' @param dat Data in data frame format.
#' @param hist A plot of histogram
#' @param boxplot  A box plot showing distribution of the variable
#' @param qqnorm A qnantile quantile plot
#' @param shapiro A  Shapiro-Wilk normality test.
#' @author Henry  Nanji, Saisakul Chernbumroong
#' @return Returns either a historgram, box-plot, q-q plot or Shapiro-Wilk statistic .
#' @keywords univariate analysis
#' @export
#' @examples
#' data(iris)
#' univariateAnalysis (iris[,-5], qqnorm  = TRUE)

univariateAnalysis  = function(dat, hist = FALSE,boxplot = FALSE,qqnorm  = FALSE, shapiro = FALSE){


  if (hist ==TRUE){


    histplot <- for(i in 1:4) {  graphics::hist(dat[,i], main=names(dat)[i], xlab = names(dat)[i], col = "skyblue")}
    return(histplot)


  }



  if(boxplot == TRUE) {


    boxplot <-  for(i in 1:4)  {graphics::boxplot(dat[,i], main=names(dat)[i], xlab = names(dat)[i], col = "skyblue")}
    return(boxplot)

  }


  if(qqnorm ==TRUE) {


    qqnormplot  <-   for(i in 1:4) {stats::qqnorm(dat[,i], main=names(dat)[i], xlab = names(dat)[i], col = "skyblue")}
    return(qqnormplot)

  }



  if(shapiro ==TRUE) {


    shp.test <- lapply(dat, stats::shapiro.test)


    shp.result <- sapply(shp.test, `[`, c("statistic","p.value"))

    return(shp.result)

  }
}
