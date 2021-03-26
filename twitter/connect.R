library(rtweet)
library(tibble)
library(tidyr)
library(stringr)
library(dplyr)
library(purrr)

appname <- "sw8"
key <- "rP5ml7aClBgBPk6Nj97X5Rqw2"
secret <- "6aYvYEdEmGcPWjDxEvl86Fjyk5Tya1dYW5N2UzssPdwtXG1k7s"
access_token <- "1693659476-QpTgGJb5iihsgXxOXW79lIGBw82kgtShlWjrxZw"
access_key <- "8w43fZc4BguNqg1bG3P4G2D3aKidwUOjVjxdFUvhGQ2U6"


twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_key
)


movies <- c("movie", "imdb", "premiere", "premier", "cinema", "theatre", "actor", "actress", "director", "movie genre")
covid <- c("covid", "covid19", "covid-sars", "mask", "wearing mask", "restriction", "virus", "vaccine", "coronavirus", "pandemic")
boardgames <- c("boardgame", "board game", "tabletop", "dicetower")
dicetower <- c("dicetower")


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

####################### search ################################
all_data <- get_lines("data/")
top_users <- all_data %>%
  group_by(screen_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  filter(n >= 5) %>%
  dplyr::select(screen_name) %>%
  unlist() %>%
  unname()

# scan_twitter(covid, path = '../../stat_kurzus/twitter/data/')
# scan_twitter(boardgames, path = '../../stat_kurzus/twitter/data/')
scan_twitter(covid)
scan_twitter(boardgames)
scan_twitter(dicetower)
scan_twitter(movies)

for (i in seq(0:7)) {
  assign(paste0("top_users_", i, "0"), top_users[seq(i * 10, i * 10 + 9)])
  scan_twitter(tokens = get(paste0("top_users_", i, "0")), name = paste0("top_users_", i, "0"))
}

# prepare_text(all_data, "all_data_test.txt")


######################## analysis ####################################

library(readr)
results <- read_csv("data/all_data_liwc_test.csv")[, -1]

replace <- function(x) {
  gsub(",", ".", x)
}
results <- map_df(results, replace)
results <- map_df(results, as.numeric)
combined_results <- bind_cols(all_data, results)

colnames <- names(combined_results)
d <- tibble(combined_results[, 101:185])
clusters <- kmeans(d, 6)

clust <- clusters$cluster

combined_results$class_pred <- clust
combined_results[, c("class", "class_pred")]

library(ggplot2)
pca <- prcomp(d)
ncomp <- 2
project <- predict(pca, d)[, 1:ncomp]

project.plus <- cbind(as.data.frame(project), cluster = combined_results$class, pred = as.factor(clust))
ggplot(project.plus, aes(PC1, PC2, color = cluster)) +
  geom_point(aes(shape = pred), size = 2, alpha = 1 / 2)
#  facet_wrap(~pred, scales = "free")


## personal functions
grouping <- c("screen_name", names(combined_results[, 101:185]))
