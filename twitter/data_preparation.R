library(tidyverse)
library(caret)
source("src/functions.R")


#### PREP STEPS
# all_data <- get_lines("twitter/data/")

# filtered_data <- all_data %>%
#   distinct(status_id, .keep_all = TRUE)


# texts <- tibble(
#   text = filtered_data$text
# ) %>%
#   mutate(id = row_number()) %>%
#   dplyr::select(id, text)

# write_csv(texts, "twitter/texts.csv")

# id_table <- filtered_data %>%
#   mutate(id = row_number())
# saveRDS(id_table, "twitter/id_table.RDS")

# liwc_results <- read_csv("twitter/liwc_results.csv")
# liwc_results <- liwc_results[-1,]
# colnames(liwc_results)[1:2] <- c("id", "text")
# write_csv(liwc_results, "twitter/liwc_results.csv")


# LIWC results refinements
# liwc_results <- read_csv("twitter/liwc_results.csv")
# liwc_results <- map_df(liwc_results[, -c(1:10)], replace)
# liwc_results <- map_df(liwc_results, as.numeric)

# liwc_results <- liwc_results[, !colnames(liwc_results) %in% c("verb", "affect", "pronoun", "ppron", "AllPunc", "function")]

# evaluate_cors(liwc_results)

# saveRDS(liwc_results, "twitter/liwc_results.RDS")

# OTHER varialbes
id_table <- readRDS("twitter/id_table.RDS")
liwc_results <- readRDS("twitter/liwc_results.RDS")
metadata <- tibble(
  id = 1:nrow(liwc_results)
)
metadata <- bind_cols(metadata, id_table[, c(1, 2, 78:82)])

mentions_length <- tibble(soc_ref = unlist(lapply(id_table$mentions_user_id, function(x) {
  length(x)
})))

metadata <- bind_cols(metadata, mentions_length)


combined_data <- liwc_results %>%
  mutate(id = row_number()) %>%
  left_join(metadata) %>%
  dplyr::select(-id)


combined_data <- combined_data %>%
  dplyr::select(user_id, status_id, colnames(combined_data)[!colnames(combined_data) %in% c("user_id", "status_id")])

top_users <- combined_data %>%
  group_by(user_id) %>%
  summarise(n = n()) %>%
  filter(n > 10)

distribution <- combined_data %>%
  filter(user_id %in% top_users$user_id) %>%
  pivot_longer(3:ncol(combined_data), names_to = "var", values_to = "val")

# set.seed(19596)
# pick <- sample(unique(distribution$user_id), 3)

# distribution %>%
#     filter(user_id %in% pick) %>%
#     ggplot() +
#     geom_histogram(aes(y=val))+
#     facet_wrap(~var)


aggregated <- distribution %>%
  group_by(user_id, var) %>%
  summarise(
    M = onestepm(val),
    m = mean(val)
  )


ggplot(aggregated) +
  geom_point(aes(M, m)) +
  facet_wrap(~var, scales = "free")

# summary(aggregated$M)
# summary(aggregated$m)

distance <- aggregated %>%
  ungroup() %>%
  dplyr::select(1, 2, 4) %>%
  spread(2:3, key = var, value = m)
df <- distance[, -1]
df <- df[, colnames(df) %in% c("social", "soc_ref")]

dist_matrix <- matrix(dist(df))
find_neighbours(dist_matrix, estimate_density = TRUE)
binned_density_data <- estimate_binned_density(df, n_bins = 10, radius_vector = c(0.005), binned_outcome = TRUE, scaling = TRUE)
binned_density_plot(binned_density_data)

exploration_plot(df, columns = c("i", "you", "we", "netspeak"))
