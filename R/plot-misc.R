#' Show base plot characters
#'
#' This function allows you to print lots of plot characters to the screen so
#' you know what you can choose from
#' @export
#' @examples
#' show_plot_chars()

show_plot_chars <- function(){
  k = 1
  plot(1:20, 1:20, type='n', axes=F, xlab="", ylab="", ylim=c(0,20), xlim=c(0,14))
  for (Y in rev(seq(1, 19, 2))) {
    for (X in 1:14) {
      text(x = X, y = Y, labels = k)
      points(x = X, y = Y + 1, pch = k)
      k = k + 1
    }
  }
}


#' Multiple plot function
#'
#' Allows one to plot multiple ggplot plot objects on the same plot.
#'
#' @param plotlist A list of ggplot objects
#' @param cols Number of columns in layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
#' @export
#' @examples
#' show_plot_chars()


multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(
      grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout)))
      )

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
