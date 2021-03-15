library(readr)
library(WRS)
library(Rcpp)
library(caret)
library(tibble)
library(tidyverse)
library(ggrepel)
library(concaveman)
library(ggforce)
# sourceCpp("../../src/functions.cpp")

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
    original <- tibble(data.frame(
      index = df[!is.na(df[variable]), names(df) == "index"],
      var = df[!is.na(df[variable]), names(df) == variable]
    ))
    colnames(original) <- c("index", variable)
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

exploration_plot <- function(dataset, id = "PIK") {
  plot_data <- dataset %>%
    pivot_longer(1:6, names_to = "var", values_to = "values") %>%
    arrange(var)

  pool <- names(dataset)
  pool1 <- pool[str_detect(pool, id)]
  pool2 <- pool[!str_detect(pool, id)]
  varnames <- tibble(expand.grid(pool1, pool2))

  plot_data <- tibble()
  for (i in 1:nrow(varnames)) {
    actual_pair <- as.character(unname(unlist(varnames[i, ])))
    actual_df <- tibble(
      var1 = rep(actual_pair[1], nrow(dataset)),
      var2 = rep(actual_pair[2], nrow(dataset)),
      x = unname(unlist(dataset[, actual_pair[1]])),
      y = unname(unlist(dataset[, actual_pair[2]]))
    )
    colnames(actual_df) <- c("var1", "var2", "x", "y")
    plot_data <- bind_rows(plot_data, actual_df)
  }

  ggplot(plot_data) +
    geom_point(aes(x, y), alpha = 1 / 10) +
    geom_smooth(aes(x, y), color = "coral4", fill = "coral3") +
    facet_wrap(~var1 ~ var2, scales = "free") +
    theme_bw() +
    labs(
      x = "Felső változó",
      y = "Alsó változó",
      caption = "A pontárnyalat a sűrűséget fejezi ki\n A vonalak illesztett LOESS trendvonalak 95% konfidencia intervallummal"
    )
}

mute <- function(exp) {
  invisible(capture.output(exp))
}

# inputVector1 <- c(1, 2, 1, 1)
# inputVector2 <- c(3, 4, 3, 2)

distance <- function(inputVector1, inputVector2, type = "ASED", all_in_table = FALSE, custom_names = NA) {
  if (!type %in% c("ASED", "SED", "ED", "Manhattan", "Csebisev", "Pearson")) {
    stop('tpye must be one of c("ASED", "SED", "ED", "Manhattan", "Csebisev", "Pearson")')
  }
  indexvector <- seq(1:length(inputVector1))
  if (type == "ASED") {
    d <- mean(sapply(indexvector, function(i) {
      (inputVector2[i] - inputVector1[i])^2
    }), na.rm = TRUE)
    form <- "("
    for (i in indexvector) {
      actual_pair <- paste0("(", inputVector2[i], " - ", inputVector1[i], ")^2")
      if (i == min(indexvector)) {
        form <- paste0(form, actual_pair)
      } else {
        form <- paste0(form, " + ", actual_pair)
      }
    }
    form <- paste0(form, ")/", length(indexvector), " = ", d)
  } else if (type == "SED") {
    d <- sum(sapply(indexvector, function(i) {
      (inputVector2[i] - inputVector1[i])^2
    }), na.rm = TRUE)
    for (i in indexvector) {
      actual_pair <- paste0("(", inputVector2[i], " - ", inputVector1[i], ")^2")
      if (i == min(indexvector)) {
        form <- actual_pair
      } else {
        form <- paste0(form, " + ", actual_pair)
      }
    }
    form <- paste0(form, " = ", d)
  } else if (type == "ED") {
    d <- sqrt(sum(sapply(indexvector, function(i) {
      (inputVector2[i] - inputVector1[i])^2
    }), na.rm = TRUE))
    form <- "SQRT("
    for (i in indexvector) {
      actual_pair <- paste0("(", inputVector2[i], " - ", inputVector1[i], ")^2")
      if (i == min(indexvector)) {
        form <- paste0(form, actual_pair)
      } else {
        form <- paste0(form, " + ", actual_pair)
      }
    }
    form <- paste0(form, ")", " = ", round(d, 3))
  } else if (type == "Manhattan") {
    d <- sum(sapply(indexvector, function(i) {
      abs(inputVector2[i] - inputVector1[i])
    }), na.rm = TRUE)
    form <- "("
    for (i in indexvector) {
      actual_pair <- paste0("|", inputVector2[i], " - ", inputVector1[i], "|")
      if (i == min(indexvector)) {
        form <- paste0(form, actual_pair)
      } else {
        form <- paste0(form, " + ", actual_pair)
      }
    }
    form <- paste0(form, ")", " = ", round(d, 3))
  } else if (type == "Csebisev") {
    d <- max(sapply(indexvector, function(i) {
      abs(inputVector2[i] - inputVector1[i])
    }), na.rm = TRUE)
    form <- "Max("
    for (i in indexvector) {
      actual_pair <- paste0("|", inputVector2[i], " - ", inputVector1[i], "|")
      if (i == min(indexvector)) {
        form <- paste0(form, actual_pair)
      } else {
        form <- paste0(form, ", ", actual_pair)
      }
    }
    form <- paste0(form, ")", " = ", round(d, 3))
  } else if (type == "Pearson") {
    p <- cor(inputVector1, inputVector2, method = "pearson")
    d <- 1 - p
    if (p < 0) {
      form <- paste0("1 - (", round(p, 3), ")")
    } else {
      form <- paste0("1 - ", round(p, 3))
    }
  }
  results <- list(d = d, type = type, formula = form)
  if (all_in_table) {
    summary_table <- tibble()
    for (m in c("ASED", "SED", "ED", "Manhattan", "Csebisev", "Pearson")) {
      dist_estimate <- distance(inputVector1, inputVector2, type = m)
      actual_estimate <- tibble(
        d = round(unname(unlist(dist_estimate["d"])), 3),
        type = as.character(unname(unlist(dist_estimate["type"]))),
        formula = as.character(unname(unlist(dist_estimate["formula"])))
      )
      summary_table <- bind_rows(summary_table, actual_estimate)
    }
    results <- summary_table
    if (!is.na(custom_names)) {
      colnames(results) <- custom_names
    }
  }
  results
}

# distance(inputVector1, inputVector2, type = "SED")
# distance(inputVector1, inputVector2, type = "ASED")
# distance(inputVector1, inputVector2, type = "ED")
# distance(inputVector1, inputVector2, type = "Manhattan")
# distance(inputVector1, inputVector2, type = "Csebisev")
# distance(inputVector1, inputVector2, all_in_table=TRUE)


KL2 <- list(
  "E" = c(3, 7),
  "F" = c(6, 7),
  "D" = c(3, 5)
)

KL3 <- list(
  "G" = c(5, 2),
  "H" = c(7, 1)
)
clusters <- list(KL2, KL3)

cluster_distance <- function(clusters, method = "min", type = "ASED", all_in_table = FALSE, rounding = 2) {
  for (c in 1:length(clusters)) {
    cluster <- clusters[c][[1]]
    members <- names(cluster)
    assign(paste0("cl", c), members)
  }

  clusterObjects <- mget(paste0("cl", 1:length(clusters)))
  pairs <- expand.grid(clusterObjects)

  find_member <- function(clusters, name) {
    for (c in 1:length(clusters)) {
      if (name %in% names(clusters[[c]])) {
        member <- clusters[[c]][names(clusters[[c]]) == name]
      }
    }
    member
  }
  compute_centroid <- function(cluster) {
    attr_length <- length(find_member(clusters, pairs[1, 1])[[1]])
    clustermatrix <- matrix(unlist(cluster), ncol = attr_length, byrow = TRUE)
    centroid <- apply(clustermatrix, 2, FUN = "mean")
    centroid
  }

  if (method != "centroid") {
    distances <- unlist(lapply(1:nrow(pairs), function(p) {
      distance(find_member(clusters, as.character(pairs[p, 1]))[[1]],
        find_member(clusters, as.character(pairs[p, 2]))[[1]],
        type = type
      )$d
    }))
  }

  if (!all_in_table) {
    if (method == "min") {
      result <- list(
          d = round(min(distances), rounding),
        pair = pairs[distances == min(distances), ],
        method = method,
        type = type
      )
    } else if (method == "max") {
      result <- list(
        d = round(max(distances), rounding),
        pair = pairs[distances == max(distances), ],
        method = method,
        type = type
      )
    } else if (method == "average") {
      result <- list(
        d = round(mean(distances), rounding),
        pair = NA,
        method = method,
        type = type
      )
    } else if (method == "centroid") {
      centroids <- lapply(1:length(clusters), function(c) {
        compute_centroid(clusters[[c]])
      })
      names(centroids) <- paste0("centroid", 1:length(clusters))
      d <- round(distance(centroids[[1]], centroids[[2]], type = type)$d, rounding)
      result <- list(
        d = d,
        pair = centroids,
        method = method,
        type = type
      )
    }
  } else {
    result <- tibble()
    for (m in c("min", "max", "average", "centroid")) {
      distance_estimate <- cluster_distance(clusters, method = m, type = type)
      if (!m %in% c("average", "centroid")) {
        estimated_pair <- unname(unlist(lapply(1:length(distance_estimate$pair), function(mem) {
          find_member(
            clusters,
            as.character(unname(distance_estimate$pair[mem])[[1]])
          )
        })))

        estimated_pair <- tibble(pair = estimated_pair, coord = c("x", "y", "xend", "yend"))
        estimated_pair <- estimated_pair %>%
          pivot_wider(names_from = coord, values_from = pair)
      }
      if (m == "centroid") {
        estimated_pair <- tibble(data.frame(matrix(unname(unlist(distance_estimate$pair)), byrow = TRUE, nrow = 1)))
        names(estimated_pair) <- c("x", "y", "xend", "yend")
      }
      if (m != "average") {
        actual_result <- tibble(
          method = m,
          d = distance_estimate$d
        )
        actual_result <- bind_cols(actual_result, estimated_pair)
      } else {
        actual_result <- tibble(
          method = m,
          d = distance_estimate$d,
          x = NA,
          y = NA,
          xend = NA,
          yend = NA
        )
      }
      result <- bind_rows(result, actual_result)
    }
  }
  result
}
# cluster_distance(clusters)
# cluster_distance(clusters, all_in_table = TRUE)
# cluster_distance(clusters, method = "max")
# cluster_distance(clusters, method = "average")
# cluster_distance(clusters, method = "centroid", type = "ASED")
# cluster_distance(clusters, method = "centroid", type = "Csebisev")
cluster_distance_plot <- function(clusters, type = "SED") {
  cluster_data <- tibble()
  for (c in 1:length(clusters)) {
    cluster <- clusters[[c]]
    members <- data.frame(matrix(unname(unlist(cluster)), byrow = TRUE, ncol = 2))
    colnames(members) <- c("x", "y")
    actual_cluster <- tibble(cluster = c)
    actual_cluster <- bind_cols(actual_cluster, members)
    actual_cluster$name <- names(cluster)
    cluster_data <- bind_rows(cluster_data, actual_cluster)
  }
  cluster_data$Cluster <- as.character(cluster_data$cluster)
  x_range <- range(cluster_data$x)
  y_range <- range(cluster_data$y)

  distance_data <- cluster_distance(clusters, all_in_table = TRUE, type = type)
  label <- ""
  for (r in 1:nrow(distance_data)) {
    label <- paste0(label, distance_data[r, ]$method, ": ", round(distance_data[r, ]$d, 3), "\n")
  }
  label_data <- tibble(x = x_range[1], y = y_range[1], label = label)

  ggplot() +
    geom_mark_hull(data = cluster_data, aes(x, y, color = Cluster, fill = Cluster, group = Cluster)) +
    geom_point(data = cluster_data, aes(x, y, color = Cluster), size = 4) +
    geom_text(data = cluster_data, aes(x, y, label = name), vjust = 1.5, hjust = 1.5, color = "black") +
    scale_color_manual(values = c("coral", "cornflowerblue")) +
    xlim(x_range[1] - x_range[1] / 2, x_range[2] + x_range[1] / 2) +
    ylim(y_range[1] - y_range[1] / 2, y_range[2] + y_range[1] / 2) +
    theme_light() +
    geom_segment(data = distance_data, aes(x = x, y = y, xend = xend, yend = yend, linetype = method), arrow = arrow(type = "closed", ends = "both", angle = 25, length = unit(0.15, "inches")), alpha = 1 / 8, show.legend = FALSE) +
    geom_point(data = distance_data[4, ], aes(x, y), shape = 13, size = 3, color = "grey50") +
    geom_text(data = distance_data[4, ], aes(x, y), label = "centroid", shape = 13, size = 4, color = "grey50", vjust = -1) +
    geom_point(data = distance_data[4, ], aes(xend, yend), shape = 13, size = 3, color = "grey50") +
    geom_text(data = distance_data[4, ], aes(xend, yend), label = "centroid", shape = 13, size = 4, color = "grey50", vjust = -1) +
    geom_text_repel(data = distance_data %>% mutate(labelx = (x + xend) / 2, labely = (y + yend) / 2), aes(labelx, labely, label = paste0(method))) +
      geom_text(data = label_data, aes(x, y, label = label)) +
    scale_fill_manual(values = c("coral", "cornflowerblue"))
}

k_means_eval <- function(dataset, k=3:5, nstart=100, iter.max=100){
  results <- tibble()
  for (j in k){
    clust <- kmeans(dataset, j, nstart = nstart, iter.max = iter.max)
    internal_criteria <-  tibble(data.frame(intCriteria(dataset, clust$cluster, "all")))
    internal_criteria$clusters <- j
    results <- bind_rows(results, internal_criteria)
  }
  results
}