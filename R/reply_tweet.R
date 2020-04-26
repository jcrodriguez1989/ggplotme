#' Replies the tweet with the new image.
#'
#' Replies the tweet with the ggplotized image.
#'
#' @param tweet A list with the tweet data.
#'
#' @importFrom dplyr `%>%`
#' @importFrom rtweet post_tweet
#' @importFrom stringr regex str_match str_squish str_to_lower
#' @importFrom utils head
#'
reply_tweet <- function(tweet) {
  img_url <- head(tweet$media_url[tweet$media_type == "photo"], 1)
  text <- tweet$text %>% str_squish()
  title <- "ggplotized by @ggplotme"
  if (grepl('with title "', text, ignore.case = TRUE)) {
    title <- str_match(
      text,
      regex('with title "(.*)"', ignore_case = TRUE)
    )[, 2]
  }
  x_points <- as.integer(
    str_match(text, regex(" (\\d+) *width", ignore_case = TRUE))[, 2]
  )
  y_points <- as.integer(
    str_match(text, regex(" (\\d+) *height", ignore_case = TRUE))[, 2]
  )
  n_colors <- as.integer(
    str_match(text, regex(" (\\d+) *colors", ignore_case = TRUE))[, 2]
  )

  new_img <- to_ggplot(img_url, x_points, y_points, n_colors, title)
  suppressMessages(post_tweet(
    status = random_message(),
    media = new_img$file,
    in_reply_to_status_id = tweet$status_id
  ))
}
