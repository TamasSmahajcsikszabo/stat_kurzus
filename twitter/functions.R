prepare_text <- function(twitter_data, title = "twitter.txt") {
  texts <- twitter_data[twitter_data$lang == "en", ]$text
  formatted <- ""
  for (text in texts) {
    formatted <- paste0(formatted, text, "\n", "===", "\n")
  }
  writeLines(formatted, title)
}

scan_twitter <- function(tokens, n = 1000, path = "", name = "", ...) {
  date <- as.character(str_sub(lubridate::now(), 1, 10))
  search_query <- paste0(tokens, collapse = " OR ")
  twitter_data <- search_tweets(search_query, n = n, token = twitter_token, include_rts = FALSE)
  twitter_data <- twitter_data[twitter_data$lang == "en", ]
  twitter_data$class <- deparse(substitute(tokens))
  if (path == "") {
    prepath <- "data/"
  } else {
    prepath <- path
  }
  if (name == "") {
    filename <- paste0(prepath, deparse(substitute(tokens)), "_", date)
  } else {
    filename <- paste0(prepath, name, "_", date)
  }
  saveRDS(twitter_data, paste0(filename, ".RDS"))
  prepare_text(twitter_data, title = paste0(filename, ".txt"))
}


get_lines <- function(path) {
  files <- list.files(path)
  files <- files[str_detect(files, "RDS")]
  results <- tibble()
  
  for (f in files) {
    actual <- readRDS(paste0(path, "/", f))
    if (!"class" %in% names(actual)) {
      actual$class <- str_sub(f, 1, str_locate(f, "_")[1, ][[1]] - 1)
    }
    results <- bind_rows(results, actual)
  }
  results
}