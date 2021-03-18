library(rtweet)
library(stringr)

appname <- "sw8"
key <- "rP5ml7aClBgBPk6Nj97X5Rqw2"
secret <- "6aYvYEdEmGcPWjDxEvl86Fjyk5Tya1dYW5N2UzssPdwtXG1k7s"
access_token="1693659476-QpTgGJb5iihsgXxOXW79lIGBw82kgtShlWjrxZw"
access_key="8w43fZc4BguNqg1bG3P4G2D3aKidwUOjVjxdFUvhGQ2U6"


twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_key)


twitter_data <- search_tweets("skywalker", n = 1000, token = twitter_token)
movies <- c("movie", "imdb", "premiere", "cinema", "theatre", "actor", "actress", "director")
covid <- c("covid", "covid19", "covid-sars", "mask", "restriction", "virus", "vaccine", "coronavirus")
boardgames <- c("bgg", "BGG", "board game", "board game geek", "table top", "tabletop", "dice", "dicetower", "card")

prepare_text <- function(twitter_data, title="twitter.txt"){
  texts <- twitter_data[twitter_data$lang=="en",]$text
  formatted <- ""
  for (text in texts){
    formatted <- paste0(formatted, text, "\n", "===", "\n")
  }
  writeLines(formatted, title)
}

scan_twitter <- function(tokens, n = 1000, ...) {
  date <- as.character(str_sub(lubridate::now(),1,10)) 
  search_query <- paste0(tokens, collapse = " OR ")
  twitter_data <- search_tweets(search_query, n = n, token = twitter_token, include_rts = FALSE)
  twitter_data <- twitter_data[twitter_data$lang == "en",]
  filename <-paste0("data/", deparse(substitute(tokens)), "_",date)
  saveRDS(twitter_data, paste0(filename, ".RDS"))
  prepare_text(twitter_data, title= paste0(filename, ".txt"))
  
}


scan_twitter(movies)
scan_twitter(covid)
scan_twitter(boardgames)




#library(readr)
#results <- read_csv("twitter_analysis.csv")
#selected <- results[, names(results) %in% c("i", "you", "we", "swear", "netspeak", "assent", "nonflu", "filler")]

