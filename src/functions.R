library(readr)
library(WRS)
library(Rcpp)
library(caret)
library(tibble)
library(tidyverse)
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

# df <- read_csv("../data/data.csv")

# ASED <- function(person1, person2) {
#   distance <- c()
#   for (i in seq_along(colnames(person1))) {
#     dist <- unname((person2[1, i][[1]] - person1[1, i][[1]])^2)
#     distance <- c(distance, dist)
#   }
#   mean(distance, na.rm = TRUE)
# }

ASED_df <- function(df) {
  mat <- as.matrix(df)
  results <- matrix(nrow = nrow(mat), ncol = nrow(mat))
  for (i in seq(nrow(mat))) {
    for (j in seq(nrow(mat))) {
      if (i == j) {
        results[i, j] <- 0
      } else {
        results[i, j] <- ASED(unlist(mat[i, ]), unlist(mat[j, ]))
      }
    }
  }
  results <- as_tibble(results)
  names(results) <- as.numeric(rownames(df))
  rownames(results) <- rownames(mat)
  results
}

find_neighbours <- function(dist_matrix, n_radius = 12, max_radius = 0.5, radius_vector = NA, summarize = FALSE, estimate_density = FALSE, cor_threshold = 0.8, preselection = FALSE) {
  ran <- range(dist_matrix)
  # zero is filterd out
  if (is.na(radius_vector)) {
    radius_vector <- seq(0, max_radius, length.out = n_radius)[-1]
    output <- matrix(nrow = nrow(dist_matrix), ncol = n_radius - 1)
  } else {
    n_radius <- length(radius_vector)
    output <- matrix(nrow = nrow(dist_matrix), ncol = n_radius)
  }

  for (p in seq(nrow(dist_matrix))) {
    for (r in seq_along(radius_vector)) {
      radius <- radius_vector[r]
      person <- unname(unlist(dist_matrix[p, ]))
      n_neighbours <- sum(person <= radius)
      output[p, r] <- n_neighbours
    }
  }
  colnames(output) <- radius_vector
  rownames(output) <- rownames(dist_matrix)

  if (!summarize) {
    result <- as_tibble(output)
  } else {
    summary <- as_tibble(output)
    summary <- summary %>%
      pivot_longer(1:all_of(n_radius), names_to = "radius", values_to = "N_neighbours") %>%
      group_by(radius) %>%
      summarize("avg_neighbours" = mean(N_neighbours)) %>%
      ungroup() %>%
      group_by(avg_neighbours) %>%
      summarize(radius = min(radius)) %>%
      dplyr::select(radius, avg_neighbours)
    result <- summary
    if (estimate_density) {
      detailed_summary <- as_tibble(output) %>%
        pivot_longer(1:all_of(n_radius), names_to = "radius", values_to = "N_neighbours") %>%
        group_by(radius) %>%
        summarize("avg_neighbours" = mean(N_neighbours))

      radius_lookup <- tibble(i = seq(n_radius), radius = as.character(radius_vector))
      suppressMessages(neighbour_data <- as_tibble(output) %>%
        mutate(person = row_number()) %>%
        pivot_longer(1:all_of(n_radius), names_to = "radius", values_to = "N_neighbours") %>%
        left_join(detailed_summary) %>%
        mutate(density = N_neighbours / avg_neighbours) %>%
        dplyr::select(person, radius, density) %>%
        left_join(radius_lookup) %>%
        mutate(density_name = paste0("Dense", i)))
      if (!preselection) {
        density_summary <- neighbour_data %>%
          # filter(radius %in% summary$radius) %>%
          group_by(density_name) %>%
          summarize(
            "avg" = mean(density),
            "std" = sqrt(var(density))
          )
      } else {
        density_summary <- neighbour_data %>%
          filter(radius %in% summary$radius) %>%
          group_by(density_name) %>%
          summarize(
            "avg" = mean(density),
            "std" = sqrt(var(density))
          )
      }
      if (!preselection) {
        density_correlations <- neighbour_data %>%
          # filter(radius %in% summary$radius) %>%
          dplyr::select(person, density_name, density) %>%
          spread(density_name, density) %>%
          dplyr::select(-person)
      } else {
        density_correlations <- neighbour_data %>%
          filter(radius %in% summary$radius) %>%
          dplyr::select(person, density_name, density) %>%
          spread(density_name, density) %>%
          dplyr::select(-person)
      }
      density_cor_matrix <- as_tibble(cor(density_correlations, method = "pearson"))
      rownames(density_cor_matrix) <- colnames(density_cor_matrix)
      cor_summary <- data.frame(matrix(nrow = nrow(density_cor_matrix), ncol = 2))
      colnames(cor_summary) <- c("Density", "NhighCorrelations")
      for (i in seq(ncol(density_cor_matrix))) {
        actual_col <- sum(unname(unlist(density_cor_matrix[, i])) >= cor_threshold)
        cor_summary[i, 1] <- colnames(density_cor_matrix[, i])
        cor_summary[i, 2] <- actual_col
      }
      suppressMessages(highest_correlations <- cor_summary %>%
        arrange(desc(NhighCorrelations)) %>%
        top_n(1))
      highest_correlations_summary <- density_summary %>%
        filter(density_name %in% highest_correlations$Density)
      result <- list("summary" = summary, "density" = density_summary, "density correaltions" = density_cor_matrix, "correlations summary" = cor_summary, "highest correlations" = highest_correlations_summary, "radius lookup" = radius_lookup)
    }
  }
  result
}

data <- dataset[, colnames(dataset) %in% c("Diener2", "Diener6")]
estimate_binned_density <- function(data, radius, n_bins = 10, radius_vector = seq(0.005, 0.5, by = 0.005)) {
  distance_matrix <- ASED_df(data)
  neighbour_matrix <- find_neighbours(distance_matrix, radius_vector = radius_vector)
  neighbour_summary <- find_neighbours(distance_matrix, radius_vector = radius_vector, summarize = TRUE)

  if (is.na(n_bins)) {
    n_bins <- range(data)[2]
  }

  bins <- tibble(bins = seq(n_bins))

  results <- bind_cols(as_tibble(data), neighbour_matrix[, names(neighbour_matrix) %in% neighbour_summary$radius])
  start_index <- ncol(data) + 1
  end_index <- ncol(results)
  results <- results %>%
    pivot_longer(start_index:end_index, names_to = "radius", values_to = "neighbours") %>%
    left_join(neighbour_summary, by = c("radius" = "radius")) %>%
    mutate(density = neighbours / avg_neighbours) %>%
    dplyr::select("radius", c(colnames(data), "density"))

  binned_density <- results %>%
    group_by_at(c("radius", colnames(data))) %>%
    summarise(density = mean(density, na.rm = TRUE)) %>%
    spread(colnames(data)[1], density)
  binned_density[is.na(binned_density)] <- 0
  binned_density
}
