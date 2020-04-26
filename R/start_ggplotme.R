#' Start ggplotme bot server.
#'
#' Starts the server, will be executing until it is aborted Ctrl+c .
#'
#' @param user A character with the bot username.
#' @param from_time A POSIXct indicating minimum tweets time to attend.
#' @param sleep_secs Numeric indicating the seconds to sleep between execution.
#'
#' @importFrom dplyr `%>%` filter
#' @importFrom rtweet search_tweets
#'
#' @export
#'
start_ggplotme <- function(
  user = "@ggplotme",
  from_time = Sys.time(),
  sleep_secs = 15 * 60) {
  created_at <- media_type <- NULL
  message(paste0("user: ", user, "\nsleep_secs: ", sleep_secs))
  while (TRUE) {
    mentions <- search_tweets(user, type = "recent", include_rts = FALSE)
    mentions <- mentions %>%
      filter(created_at > from_time) %>%
      filter("photo" %in% media_type)
    message(paste0(Sys.time(), " - ", nrow(mentions), " tweets to reply."))
    from_time <- Sys.time()
    if (nrow(mentions) > 0) {
      from_time <- max(mentions$created_at)
      apply(mentions, 1, reply_tweet)
    }
    Sys.sleep(sleep_secs)
  }
}
