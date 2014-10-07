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
