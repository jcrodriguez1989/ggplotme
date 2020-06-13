#' Start ggplotme bot server.
#'
#' Starts the server, will be executing until it is aborted Ctrl+c .
#'
#' @param user A character with the bot username.
#' @param from_time A POSIXct indicating minimum tweets time to attend.
#' @param sleep_secs Numeric indicating the seconds to sleep between execution.
#' @param rtweet_token_file A character with the file path of an rds file with
#'   the rtweet token to be used. If NULL, it will load the default token.
#'
#' @importFrom dplyr `%>%` filter
#' @importFrom rtweet search_tweets
#'
#' @export
#'
start_ggplotme <- function(
  user = "@ggplotme",
  from_time = Sys.time(),
  sleep_secs = 15 * 60,
  rtweet_token_file = NULL
) {
  if (!is.null(rtweet_token_file)) {
    assign(
      "twitter_tokens",
      readRDS(rtweet_token_file),
      envir = rtweet:::.state
    )
  }
  created_at <- media_type <- NULL
  message(paste0("user: ", user, "\nsleep_secs: ", sleep_secs))
  while (TRUE) {
    mentions <- try({
      search_tweets(user, type = "recent", include_rts = FALSE) %>%
        filter(created_at > from_time) %>%
        filter("photo" %in% media_type)
    })
    if (inherits(mentions, "try-error")) {
      mentions <- data.frame()
    }
    message(paste0(Sys.time(), " - ", nrow(mentions), " tweets to reply."))
    from_time <- Sys.time()
    if (nrow(mentions) > 0) {
      from_time <- max(mentions$created_at)
      apply(mentions, 1, reply_tweet)
    }
    Sys.sleep(sleep_secs)
  }
}
