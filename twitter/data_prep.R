source("../src/functions.R")
dataset <- readRDS("prepared_data.RDS")
dataset <- dataset %>% dplyr::select(-AllPunc, -Analytic, -Clout, -Authentic, -Tone, -Sixltr)
liwc <- dataset[, seq(1:length(names(dataset)))[names(dataset) == "i"]:length(names(dataset))]

# liwc <- dataset[, 98:179]
# liwc <- tibble(data.frame(lapply(liwc, function(x) {
#   as.numeric(replace(x))
# })))
# densityMatrix(liwc)
# dataset[,98:179] <- liwc
# saveRDS(dataset, "prepared_data.RDS")


# least variance

# skew is low

# curtosis is low

dataset <- liwc

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
    summarise(votes = sum(ishighR)) %>%
    arrange(desc(votes)) %>%
    filter(votes != 0)
  for (name in unique(high_correlations$var)) {
    votes[votes$var == name, ]$votes <- votes[votes$var == name, ]$votes + high_correlations[high_correlations$var == name, ]$votes
  votes[votes$var ==name, ]$corr <- high_correlations[high_correlations$var ==name, ]$votes
  }

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
    filter(isZero > missingThreshold)

  for (name in unique(missing$var)) {
    votes[votes$var == name, ]$votes <- votes[votes$var == name, ]$votes + floor(nrow(dataset) / (missingThreshold))
    votes[votes$var ==name, ]$missing <- high_correlations[high_correlations$var ==name, ]$votes
  }

  # 3. least variance
  variance <- dataset %>% pivot_longer(1:ncol(dataset), names_to = "var", values_to = "val")
  variance <- variance %>%
    group_by(var) %>%
    summarise(v = var(val, na.rm = TRUE)) %>%
    mutate(votes = (max(v) - v) / max(v)) %>%
    filter(votes > 0)

  for (name in unique(variance$var)) {
    votes[votes$var == name, ]$votes <- votes[votes$var == name, ]$votes + 1
  }

  votes %>% arrange(desc(votes))
}
evaluate_data(liwc)
