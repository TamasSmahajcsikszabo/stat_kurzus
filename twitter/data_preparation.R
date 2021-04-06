source("twitter/functions.R")
all_data <- get_lines("twitter/data/")

tweet_id <- unique(all_data$status_id)
filtered_data <- all_data %>% 
  filter(status_id %in% tweet_id)
