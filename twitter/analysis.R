library(tidytext)
source("../src/functions.R")
dataset <- readRDS("prepared_data.RDS")


scenario <- "movies"
selected_data <- dataset %>% filter(class == scenario)

selected_data <- selected_data %>% dplyr::select(-AllPunc, -Analytic, -Clout, -Authentic, -Tone, -Sixltr, -OtherP, -Exclam, -Period, -QMark)

selected_data <- selected_data %>%
  filter(!is_quote == TRUE) %>%
  filter(!is_retweet == TRUE)

liwc <- selected_data[, c(2, seq(1:length(names(dataset)))[names(dataset) == "i"]:length(names(selected_data)))]

selected_cols <- evaluate_data(liwc[, 2:ncol(liwc)]) %>%
  filter(is.na(R)) %>%
  arrange(votes, desc(missing_rate))

selected_cols <- selected_cols$var[1:8]

selected_data <- liwc[, c("status_id", selected_cols)]

# filter out outliers
for (name in names(selected_data)[names(selected_data) != "status_id"]) {
  outliers <- unlist(MAD(selected_data[name][[1]])["outlier index"])
  print(paste0("Filtered ", str(sum(outliers)), " outlier for var ", str(name)))
  selected_data <- selected_data[!outliers, ]
}

# densityMatrix(selected_data)

# clustering
mclust_results <- autocluster(selected_data[1:1500, 2:9], method = "mclust", k_range = 2:5, PCA = TRUE, scaling = FALSE)
mclust_results["BIC plot"]
mclust_results["ICL plot"]
mclust_results["boundary plot"]
mclust_results["PCA plot"]
mclust_results["best k"]
mclust_results["centers"]
mclust_results["density plot"]
mclust_results["hard membership"]

membership <- bind_cols(selected_data[1:1500, ], mclust_results["hard membership"][[1]])

# frequent terms per cluster

texts <- dataset %>%
  filter(status_id %in% membership$status_id) %>%
  select(status_id, text.x) %>%
  unnest_tokens(words, text.x) %>%
  filter(!words %in% stop_words$word) %>%
  left_join(membership[, c(1, 10, 11)]) %>%
  group_by(c, words) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n))



texts %>%
  filter(n > 20) %>%
  ggplot() +
  geom_col(aes(words, n)) +
  coord_flip() +
  facet_wrap(~c)
