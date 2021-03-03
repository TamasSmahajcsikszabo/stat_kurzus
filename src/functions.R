library(readr)
library(WRS)
library(Rcpp)
library(caret)
library(tibble)
library(tidyverse)
library(ggrepel)
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
  dist_var <- var(as.matrix(dist_matrix)[lower.tri(as.matrix(dist_matrix))], na.rm = FALSE)
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
      n_neighbours <- sum(person <= (radius * dist_var))
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
    pivot_longer(all_of(start_index):all_of(end_index), names_to = "radius", values_to = "neighbours") %>%
    left_join(neighbour_summary, by = c("radius" = "radius")) %>%
    mutate(density = neighbours / avg_neighbours) %>%
    dplyr::select("radius", c(colnames(data), "density"))

  suppressMessages(binned_density <- results %>%
    group_by_at(c("radius", colnames(data))) %>%
    summarise(density = mean(density, na.rm = TRUE)))
  binned_density[is.na(binned_density)] <- 0
  binned_density
}
# binned_data <- estimate_binned_density(data)
# binned_density_data <- binned_data

binned_density_plot <- function(binned_density_data, title = "", xlab = "", ylab = "", caption = "") {
  colnames(binned_density_data) <- c("radius", "x", "y", "density")
  x_breaks <- seq(max(binned_density_data$x))
  y_breaks <- seq(max(binned_density_data$y))
  ggplot(data = binned_density_data) +
    geom_tile(aes(x = x, y = y, fill = density), color = "grey50") +
    scale_fill_gradient(low = "grey90", high = "coral") +
    geom_text(aes(x, y, label = round(density, 3))) +
    facet_wrap(~radius, ncol = 2) +
    labs(
      title = title,
      x = xlab,
      y = ylab,
      caption = caption
    ) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    theme_light() +
    theme(legend.position = "bottom")
}

# binned_density_plot(binned_density_data, title = "Diener2 es Diener6 surusodespontjai 2 radius ertek menten", xlab = "Diener2", ylab = "Diener6", caption = "Atlag surusodes ertekek")

binned_density_frequency <- function(binned_density_data) {
  binned_density_data %>%
    group_by(radius, density) %>%
    summarise(frequency = n()) %>%
    arrange(radius, desc(density))
}
get_upper_matrix <- function(mat) {
  values <- c()
  for (i in seq(2, ncol(mat))) {
    for (j in seq(i - 1)) {
      values <- c(values, mat[i, j])
    }
  }
  values
}

# data <- read_csv("../../data/data.csv")
# data <- data[, names(data) %in% paste0("Diener", c("2", "6"))]
# threshold <- 0.025
# upper_bound <- 50
# merge <- 0.05
# max_dense <- 15


get_dense_points <- function(data, initial_max_points = 200, threshold = 0.25, merge = 0.05, max_dense = 15, upper_bound = 100, columns = c("Diener2", "Diener6")) {

  # ASED variance
  ASED_matrix <- ASED_df(data)

  # identify neighbours and see their frequency
  neighbour_matrix <- find_neighbours(ASED_matrix, radius_vector = c(threshold))
  colnames(neighbour_matrix) <- "N"
  neighbour_matrix <- neighbour_matrix %>%
    mutate(id = row_number()) %>%
    arrange(desc(N)) %>%
    top_n(N, initial_max_points) %>%
    filter(N >= upper_bound)

  # create initial DP data
  top_person <- neighbour_matrix$id
  DP <- data %>%
    mutate(id = row_number()) %>%
    filter(id %in% all_of(top_person))
  left_out <- data %>%
    mutate(id = row_number()) %>%
    filter(!id %in% all_of(top_person))

  # merge

  dist_var <- var(as.matrix(dist_matrix)[lower.tri(as.matrix(ASED_matrix))], na.rm = FALSE)
  merge_distance <- dist_var * merge
  DP_count <- nrow(DP)
  DP_stored <- tibble()
  run <- 0

  while (DP_count > max_dense) {
    run <- run + 1
    DP_distance <- ASED_df(DP[, c(1, 2)])
    DP_size <- nrow(DP)
    neighbours <- find_neighbours(DP_distance, radius_vector = c(merge))
    colnames(neighbours) <- "N"
    if ("N" %in% names(DP)) {
      DP <- DP[, 1:3]
    }
    neighbours <- bind_cols(DP, neighbours) %>%
      arrange(desc(N))
    top_N <- max(neighbours$N)
    to_merge_id <- neighbours[neighbours$N == all_of(top_N), ]$id
    left_over_DP <- neighbours %>%
      filter(!id %in% all_of(to_merge_id))
    to_merge_data <- neighbours %>%
      filter(id %in% all_of(to_merge_id)) %>%
      summarise_(
        aggregated = mean(DP[columns[1]][[1]], na.rm = TRUE),
        aggregated2 = mean(DP[columns[2]][[1]], na.rm = TRUE)
      ) %>%
      rename_(.dots = setNames("aggregated", paste0(columns[1]))) %>%
      rename_(.dots = setNames("aggregated2", paste0(columns[2]))) %>%
      mutate(id = run)
    DP_stored <- bind_rows(DP_stored, to_merge_data)
    if (nrow(DP_stored) >= max_dense) {
      break
    }
    DP <- left_over_DP
    DP_new_size <- nrow(DP)
    if (DP_size == DP_new_size) {
      break
    }
    DP_count <- nrow(DP)
  }
  new_data <- bind_rows(left_out, left_over_DP, DP_stored)

  list("dense point" = DP_stored, "new data" = new_data, "left out" = left_out)
}


dense_point_plot <- function(data, xlab = "Diener2", ylab = "Diener6", max_dense = 15) {
  plot_data <- data
  colnames(plot_data) <- c("x", "y")
  x_breaks <- seq(max(plot_data$x))
  y_breaks <- seq(max(plot_data$y))
  dense_points <- get_dense_points(data, max_dense = max_dense)
  dense_point_data <- dense_points[1][[1]]
  colnames(dense_point_data) <- c("x", "y", "id")

  suppressMessages(ggplot() +
    geom_jitter(data = plot_data, aes(x, y), alpha = 1 / 5, size = 2) +
    geom_point(data = dense_point_data, aes(x, y), color = "grey30", size = 5) +
    geom_point(data = dense_point_data, aes(x, y), color = "coral", size = 4) +
    geom_text(data = dense_point_data, aes(x, y, label = id), color = "black", size = 4) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(
      x = xlab,
      y = ylab
    ) +
    theme_light() +
    xlim(3, 7) +
    ylim(3, 7))
}
# dense_point_plot(data, 15)
