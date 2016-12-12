#' Create a color checker chart
#'
#' Color-checker-chart ggplot labelled with color names or with indexes of the
#' colors in the vector passed as first argument.
#'
#' @param colors character A vector of color definitions.
#' @param ncol integer Number of column in the checker grid.
#' @param use.names logical Force use of names or indexes.
#' @param text.size numeric Size of the text labels drawn on each color patch.
#'
#' @export
#'
#' @examples
#'
#' ggcolorchart()
#' ggcolorchart(grep("dark", colors(), value = TRUE))
#'
ggcolorchart <- function(colors = grDevices::colors(),
                         ncol = NULL,
                         use.names = NULL,
                         text.size = 2) {
  # needed if the argument passed is subset with [ ]!
  force(colors)

  len.colors <- length(colors)
  # use squarish tiles by default
  if (is.null(ncol)) {
    ncol <- max(trunc(sqrt(len.colors)), 1L)
  }
  # use color names for seven or fewer columns by default
  if (is.null(use.names)) {
    use.names <- ncol < 8
  }
  # number of rows needed to fit all colors
  nrow <- len.colors %/% ncol
  if (len.colors %% ncol != 0) {
    nrow <- nrow + 1
  }
  # we extend the vector with NAs to match number of tiles
  if (len.colors < ncol*nrow) {
    colors[(len.colors + 1):(ncol*nrow)] <- NA
  }
  # we build a data frame
  colors.df <-
    data.frame(color = colors,
               text.color = black_or_white(colors),
               x = rep(1:ncol, nrow),
               y = rep(nrow:1, rep(ncol, nrow)),
               idx = ifelse(is.na(colors),
                            "",
                            format(1:(ncol * nrow), trim = TRUE)))
  # we build the plot
  p <- ggplot(colors.df, aes(x, y, fill = color))
  if (use.names) {
    p <- p + aes(label = ifelse(is.na(colors), "", colors))
  } else {
    p <- p + aes(label = format(idx, width = 3))
  }
  p <- p +
    geom_tile(color = "white") +
    scale_fill_identity() +
    geom_text(size = text.size, aes(color = text.color)) +
    scale_color_identity()
  p + theme_void()
}

#' Chose black vs. white color based on mean of RGB channels
#'
#' Chose black or white color based on a color to be used as background.
#' Usefull when using \code{geom_text} on top of tiles or bars, or
#' \code{geom_label} with a variable fill.
#'
#' @param colors character A vector of color definitions.
#' @param threshold numeric in range 0 to 1.
#'
#' @export
#'
#' @examples
#'
#' black_or_white("red")
#' black_or_white(colors()[1:10])
#'
black_or_white <- function(colors, threshold = 0.5){
  threshold <- trunc(threshold * 255)
  ifelse(sapply(colors,
                function(x){mean(col2rgb(x))}) > threshold,
         "black", "white")
}
