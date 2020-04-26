#' Returns a ggplotized image.
#'
#' Returns the png saved file of the generated ggplot.
#'
#' @param img_url A character with the path of the image to convert.
#' @param x_points A numeric indicating the new image width.
#' @param y_points A numeric indicating the new image height.
#' @param n_colors A numeric indicating the number of colors to use in the new
#'   image.
#' @param title A character with the title to use in the new image.
#'
#' @importFrom dplyr `%>%` mutate select
#' @importFrom ggplot2 ggplot aes geom_raster scale_fill_identity xlab ylab
#' @importFrom ggplot2 ggtitle ggsave
#' @importFrom grDevices rgb
#' @importFrom imager load.image resize
#' @importFrom stats kmeans
#'
#' @export
#'
to_ggplot <- function(img_url, x_points = NA, y_points = NA, n_colors = NA,
                      title = "ggplotized by @ggplotme") {
  c.1 <- c.2 <- c.3 <- x <- y <- rgb.val <- NULL
  img_dframe <- load.image(img_url) # load image
  # if it is NULL, keep original size for x
  if (is.na(x_points)) {
    x_points <- nrow(img_dframe)
  }
  # if it is NULL, keep original size for y
  if (is.na(y_points)) {
    y_points <- ncol(img_dframe)
  }
  img_dframe <- img_dframe %>%
    resize(x_points, y_points) %>%
    as.data.frame(wide = "c") %>%
    mutate(y = max(y) - y) # reverse y points (but not the y scale in the plot)
  if (!is.na(n_colors)) {
    # cluster colors into n_colors groups
    color_clusts <- img_dframe %>%
      select(c.1, c.2, c.3) %>%
      kmeans(n_colors)
    img_dframe$rgb.val <- rgb(color_clusts$centers[color_clusts$cluster, ])
  } else {
    img_dframe$rgb.val <- rgb(select(img_dframe, c.1, c.2, c.3))
  }
  ggp <- ggplot(img_dframe, aes(x, y)) +
    geom_raster(aes(fill = rgb.val)) +
    scale_fill_identity() +
    xlab("") +
    ylab("") +
    ggtitle(title)
  out_file <- tempfile(fileext = ".png")
  suppressMessages(ggsave(out_file, ggp))
  out_file
}
