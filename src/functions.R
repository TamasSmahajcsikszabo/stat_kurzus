library(readr)
library(WRS)
library(Rcpp)
library(caret)
sourceCpp("../../src/functions.cpp")

get_tau <- function(x, y, algorithm = "Kendall", verbose = TRUE) {
  cd_est <- CD(x, y)
  P <- cd_est[1]
  Q <- cd_est[2]
  T <- cd_est[3]
  U <- cd_est[4]
  if (algorithm == "Kendall") {
    n <- length(x)
    denom <- n * (n - 1) / 2
  } else if (algorithm == "Knight") {
    denom <- sqrt((P + Q + T) * (P + Q + U))
  }
  tau <- (P - Q) / denom
  if (verbose) {
    tau <- list(
      "tau" = tau,
      "concordants" = P,
      "discordants" = Q,
      "p+" = P / denom,
      "p-" = Q / denom
    )
  }
  tau
}

summarize_custom <- function(df, FUN = "is.na", names = NA) {
  results <- data.frame(matrix(ncol = 2, nrow = ncol(df)))
  names(results) <- c("variable", "n")

  for (n in seq_along(colnames(df))) {
    actual_variable <- unname(unlist(df[n]))
    result <- sum(do.call(FUN, list(actual_variable)))
    results[n, 1] <- names(df)[n]
    results[n, 2] <- result
  }
  if (!is.na(names[1])) {
    names(results) <- names
  }
  results
}

filter_missing_data <- function(df) {
  df$full <- apply(df, MARGIN = 1, function(x) sum(is.na(x))) == 0
  df <- df %>%
    filter(full)
  df <- df %>%
    dplyr::select(-full)
  df
}

# y_names <- names(missing_subset)
# df <- dataset
# df[df$index == 1443,]['Fizerő']

impute_missing_values <- function(df, y_names = c(), repeats = 0, runname = repeats) {
  train_data <- df %>%
    filter_missing_data()

  for (variable in y_names) {
    # print(paste0("Run ", runname, " with ", variable))

    x <- train_data[!names(train_data) %in% c(variable, y_names)]
    y <- unname(unlist(train_data[names(train_data) == variable]))
    x_new <- df[is.na(df[variable]), !names(df) %in% c(variable, y_names)]
    knnmodel <- train(x, y,
      method = "knn",
      preProc = c("center", "scale")
    )

    predicted <- floor(predict(knnmodel, x_new))
    index <- x_new$index
    original <- data.frame(
      index = df[!is.na(df[variable]), names(df) == "index"],
      var = df[!is.na(df[variable]), names(df) == variable]
    )
    names(original) <- c("index", variable)
    new_data <- data.frame(index = index, pred = predicted)
    names(new_data) <- c("index", variable)
    full <- original %>% bind_rows(new_data)
    names(full) <- c("index", paste0(variable, "_imputed"))
    df <- df %>% left_join(full)
  }
  if (repeats > 0) {
    for (i in seq(1, repeats)) {
      impute_missing_values(df, y_names = y_names, repeats = 0, runname = i + 1)
    }
  }
  df
}

find_pairs <- function(vec) {
  pairs <- data.frame()
  for (i in seq_along(vec)) {
    for (j in seq_along(vec)) {
      if (i != j) {
        token1 <- vec[i]
        token2 <- vec[j]
        if (stringr::str_detect(token1, token2)) {
          actual_pair <- data.frame(p1 = token1, p2 = token2)
          pairs <- bind_rows(pairs, actual_pair)
        }
      }
    }
  }
  pairs
}

impute_plot <- function(df, y_names) {
  dfpairs <- find_pairs(names(df)) %>%
    filter(p2 %in% y_names)

  pair_data <- tibble()
  layer1 <- tibble()
  layer2 <- tibble()
  layer3 <- tibble()
  layer4 <- tibble()

  for (p in seq(nrow(dfpairs))) {
    pair_name <- unname(unlist(dfpairs[p, ]))
    pair_title <- paste0(pair_name, collapse = " & ")
    pair_data <- df %>%
      dplyr::select(all_of(pair_name)) %>%
      mutate(index = row_number()) %>%
      gather(1:2, key = "var", value = "value") %>%
      mutate(value = if_else(is.na(value), 10, value)) %>%
      mutate(gr = pair_title)
    actual_layer1 <- pair_data[pair_data$var == pair_name[2], ] %>%
      filter(value < 10) %>%
      mutate(gr = pair_title)
    actual_layer2 <- pair_data[pair_data$var == pair_name[2], ] %>%
      filter(value == 10) %>%
      mutate(gr = pair_title)

    actual_layer3 <- pair_data[pair_data$var == pair_name[1], ] %>%
      mutate(gr = pair_title)
    actual_layer4 <- actual_layer3[actual_layer3$index %in% actual_layer2$index, ] %>%
      mutate(gr = pair_title)
    layer1 <- bind_rows(layer1, actual_layer1)
    layer2 <- bind_rows(layer2, actual_layer2)
    layer3 <- bind_rows(layer3, actual_layer3)
    layer4 <- bind_rows(layer4, actual_layer4)
  }
  layer1 <- layer1 %>%
    mutate(layer = "layer1")
  layer2 <- layer2 %>%
    mutate(layer = "layer2")
  layer3 <- layer3 %>%
    mutate(layer = "layer3")
  layer4 <- layer4 %>%
    mutate(layer = "layer4")
  plotdata <- bind_rows(layer1, layer2, layer3, layer4)

  ggplot() +
    geom_point(
      data = plotdata[plotdata$layer == "layer1", ], aes(
        x = index,
        y = value
      ), color = "grey60", size = 3
    ) +
    geom_point(data = plotdata[plotdata$layer == "layer2", ], aes(x = index, y = value), color = "coral", size = 5) +
    geom_point(data = plotdata[plotdata$layer == "layer4", ], aes(x = index, y = value), shape = 18, size = 5) +
    geom_segment(data = plotdata[plotdata$layer %in% c("layer2", "layer4"), ] %>% dplyr::select(-var) %>% spread(layer, value), aes(x = index, xend = index, y = layer2, yend = layer4), arrow = arrow()) +
    labs(
      title = paste0("Valtozók imputációja"),
      x = "A személy indexe",
      y = "A változó értéke"
    ) +
    theme_light() +
    facet_wrap(~gr)
}
