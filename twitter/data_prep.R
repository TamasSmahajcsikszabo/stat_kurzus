source("../src/functions.R")
dataset <- readRDS("prepared_data.RDS")


# top_2000_users <- dataset %>%
#   group_by(user_id) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n)) %>%
#   top_n(5000, n) %>%
#   select(user_id) %>%
#   unlist()

dataset <- dataset %>% dplyr::select(-AllPunc, -Analytic, -Clout, -Authentic, -Tone, -Sixltr, -OtherP, -Exclam, -Period, -QMark)
# take out retweets and quotes
dataset <- dataset %>%
  filter(!is_quote == TRUE) %>%
  filter(!is_retweet == TRUE)

per_user <- dataset %>%
  group_by(user_id) %>%
  summarise(n = n()) %>%
  filter(n >= 5) %>%
  select(user_id) %>%
  unlist()

dataset <- dataset %>%
  filter(user_id %in% per_user)

liwc <- dataset[, seq(1:length(names(dataset)))[names(dataset) == "i"]:length(names(dataset))]




liwc_m <- bind_cols(tibble(ID = dataset$user_id), liwc)
liwc_m <- liwc_m %>%
  pivot_longer(2:ncol(liwc), names_to = "var", values_to = "val") %>%
  filter(val > 0) %>%
  group_by(ID, var) %>%
  summarise(val = onestepm(val))

liwc_wide_m <- liwc_m %>%
  ungroup() %>%
  pivot_wider(names_from = "var", values_from = "val")
liwc_wide_m[is.na(liwc_wide_m)] <- 0.0
saveRDS(liwc_wide_m, "liwc_wide_m.RDS")

liwc_s <- bind_cols(tibble(ID = dataset$user_id), liwc)
liwc_s <- liwc_s %>%
  pivot_longer(2:ncol(liwc), names_to = "var", values_to = "val") %>%
  filter(val > 0) %>%
  group_by(ID, var) %>%
  summarise(val = mad(val, na.rm = TRUE))

liwc_wide_s <- liwc_s %>%
  ungroup() %>%
  pivot_wider(names_from = "var", values_from = "val")
liwc_wide_s[is.na(liwc_wide_s)] <- 0.0
colnames(liwc_wide_s)[2:ncol(liwc_wide_s)] <- paste0(colnames(liwc_wide_s)[2:ncol(liwc_wide_s)], "_s")
saveRDS(liwc_wide_s, "liwc_wide_s.RDS")

# LIWC indicators plus metadata
metadata <- dataset %>%
  rowwise() %>%
  mutate(soc_Ref = length(mentions_screen_name)) %>%
  group_by(ID = user_id) %>%
  summarise(
    favourites = max(favorite_count, na.rm = TRUE),
    favourites_s = mad(favorite_count),
    retweets = max(retweet_count, na.rm = TRUE),
    retweets_s = mad(retweet_count, na.rm = TRUE),
    followers = max(followers_count, na.rm = TRUE),
    followers_s = mad(followers_count, na.rm = TRUE),
    friends = max(friends_count, na.rm = TRUE),
    friends_s = mad(friends_count, na.rm = TRUE),
    statuses = max(statuses_count, na.rm = TRUE),
    statuses_s = mad(statuses_count, na.rm = TRUE),
    soc_ref = max(soc_Ref),
    soc_ref_s = mad(soc_Ref),
  )

final_data <- liwc_wide_m
final_data <- liwc
# left_join(liwc_wide_s) %>%
# left_join(metadata)

evaluate_data <- function(dataset, Rlimit = 0.7, missingThreshold = floor(nrow(dataset) / 10), ...) {
  votes <- tibble(var = names(dataset), votes = rep(0, length(names(dataset))))

  # 1. too many correlations
  covar <- tibble(data.frame(cor(dataset)))
  covar <- bind_cols(tibble(var = names(dataset)), covar)
  covar <- covar %>% pivot_longer(2:ncol(covar), names_to = "correlate", values_to = "R")
  covar <- covar %>% filter(R != 1)
  high_correlations <- covar %>%
    mutate(ishighR = if_else(R >= Rlimit, 1, 0)) %>%
    group_by(var) %>%
    summarise(R = sum(ishighR)) %>%
    arrange(desc(R)) %>%
    filter(R != 0)
  for (name in unique(high_correlations$var)) {
    votes[votes$var == name, ]$votes <- votes[votes$var == name, ]$votes + high_correlations[high_correlations$var == name, ]$R
  }
  votes <- votes %>% left_join(high_correlations)

  # 2. too many missing or zero
  missing <- dataset %>% pivot_longer(1:ncol(dataset), names_to = "var", values_to = "val")
  missing <- missing %>%
    mutate(isZero = if_else(val == 0, 1, 0))
  missing <- missing %>%
    group_by(var) %>%
    summarise(
      isZero = sum(isZero)
    ) %>%
    filter(isZero > 0) %>%
    filter(isZero > missingThreshold) %>%
    mutate(missing_rate = isZero / missingThreshold)

  for (name in unique(missing$var)) {
    votes[votes$var == name, ]$votes <- votes[votes$var == name, ]$votes + missing[missing$var == name, ]$missing_rate
  }
  votes <- votes %>% left_join(missing[, c(1, 3)])

  # 3. least variance
  variance <- dataset %>% pivot_longer(1:ncol(dataset), names_to = "var", values_to = "val")
  maxv <- var(variance$val)
  variance <- variance %>%
    group_by(var) %>%
    summarise(v = var(val, na.rm = TRUE)) %>%
    mutate(varvote = (maxv - v) / maxv)

  for (name in unique(variance$var)) {
    votes[votes$var == name, ]$votes <- votes[votes$var == name, ]$votes + variance[variance$var == name, ]$varvote
  }
  votes <- votes %>% left_join(variance[, c(1, 3)])

  votes %>% arrange(votes, desc(T), desc(missing_rate), desc(varvote))
}
evaluate_data(final_data[2:ncol(final_data)])
selected_cols <- evaluate_data(final_data) %>%
  filter(is.na(R)) %>%
  arrange(votes, desc(missing_rate))

selected_cols <- selected_cols$var[1:5]
selected_cols <- c("i", "we", "you", "ipron", "prep", "article", "Exclam", "Period")
selected_data <- final_data[, selected_cols]

# filter out outliers
for (name in names(selected_data)) {
  outliers <- unlist(MAD(selected_data[name][[1]])["outlier index"])
  print(paste0("Filtered ", str(sum(outliers)), " outlier for var ", str(name)))
  selected_data <- selected_data[!outliers, ]
}

densityMatrix(selected_data)

set.seed(341)
index <- sample(1:nrow(selected_data), 2000)

mclust_results <- autocluster(selected_data[index, ], method = "mclust", k_range = 2:5, PCA = TRUE, scaling = FALSE)
mclust_results["BIC plot"]
mclust_results["ICL plot"]
mclust_results["boundary plot"]
mclust_results["PCA plot"]
mclust_results["best k"]
mclust_results["centers"]
mclust_results["density plot"]
plot(mclust_results["mclust fit"])


saveRDS(mclust_results, "MKA_base.RDS")


# level set tree clustering with knn density estimation
library(denpro)
library(TDA) # kernel density estimation
dens <- knnDE(selected_data[, 1], selected_data[, 2], k = 25)
ggplot(data = selected_data) +
  geom_point(aes(x = prep, y = cogproc, alpha = dens))

pcf <- pcf.kern(selected_data[, c(1, 2)], h = 0.5, N = c(32, 32))
lst <- leafsfirst(pcf) # level set tree
td <- treedisc(lst, pcf, ngrid = 60) # pruned level set tree
cd <- colors2data(selected_data[, c(1, 2)], pcf, lst)

ggplot(data = selected_data) +
  geom_point(aes(x = prep, y = cogproc), shape = 1, size = 4, alpha = 1 / 2) +
  geom_point(aes(x = prep, y = cogproc, color = unlist(cd[1])), size = 3) +
  scale_color_manual(values = get_color_scale(56))

est_bin <- estimate_binned_density(selected_data[1:2000, 1:2], n_bins = 10, radius_vector = c(0.5), scaling = TRUE, binned_outcome = TRUE)
binned_density_plot(est_bin)
