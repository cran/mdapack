#' A plotHeatmapGroup function for numeric variables
#'
#' 'plotHeatmapGroup' returns a plot showing the strength of correlation of numeric variables in a data frame.
#'  The function receives just one input as data frame.
#'
#' @param dat Data in data frame format.
#' @param ... Optional arguments
#' @author Henry  Nanji,Saisakul Chernbumroong
#' @return A heatmap is returned showing correlation of all numeric variables in data frame
#' @keywords heatmap
#' @export
#' @examples
#' data(iris)
#'plotHeatmapGroup(iris[,1:4])
plotHeatmapGroup = function(dat,...)

  {

  if(requireNamespace("ggplot2", quietly = TRUE))

    if(requireNamespace("reshape2", quietly = TRUE))

      {

        dat <- dat[,1:4]
        cor <- reshape2::melt(cor(dat, use="p"))

        heatmap1 <- ggplot2::ggplot(data=cor, ggplot2::aes(x=cor[,1], y=cor[,2], fill=cor[,3]))
        heatmap2 <-heatmap1 + ggplot2::geom_tile() + ggplot2::labs(x = "", y = "") + ggplot2::scale_fill_gradient2(limits=c(-1, 1))

        return(heatmap2)

      }

}
