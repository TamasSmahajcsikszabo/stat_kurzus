# MAD based estimation for outliers
library(ggplot2)
library(tidyverse)


MAD <- function(x, verbose = TRUE) {
  med <- median(x, na.rm = TRUE)
  med_diff <- c()
  for (i in seq_along(x)) {
    d <- abs(x[i] - med)
    med_diff[i] <- d
  }
  MAD <- median(med_diff, na.rm = TRUE)
  s <- MAD / 0.6745
  outlier_crit <- 2 * s
  outlier_vector <- c()
  for (i in seq_along(x)) {
    if (abs(x[i] - med) > outlier_crit) {
      outlier_vector[i] <- TRUE
    } else {
      outlier_vector[i] <- FALSE
    }
  }

  if (verbose) {
    list(
      "MAD" = MAD,
      "s" = s,
      "outlier criteria" = outlier_crit,
      "outliers" = x[outlier_vector],
      "outlier index" = outlier_vector
    )
  } else {
    MAD
  }
}

x <- c(34, 49, 49, 44, 66, 48, 49, 39, 54, 57, 39, 65, 43, 43, 44, 42, 71, 40, 41, 38, 42, 77, 40, 38, 43, 42, 36, 55, 57, 57, 41, 66, 69, 38, 49, 51, 45, 141, 133, 76, 44, 40, 56, 50, 75, 44, 181, 45, 61, 15, 23, 42, 61, 146, 144, 89, 71, 83, 49, 43, 68, 57, 60, 56, 63, 136, 49, 57, 64, 43, 71, 38, 74, 84, 75, 64, 48)

MAD_plot <- function(x) {
  estimates <- MAD(x)
  plot_data <- data.frame(x)
  plot_data$index <- seq_along(x)
  outlier_vector <- estimates[5]
  plot_data <- cbind(plot_data, outlier = outlier_vector)
  colnames(plot_data) <- c("x", "index", "outlier")
  plot_data[plot_data$outlier == TRUE, 3] <- plot_data[plot_data$outlier == TRUE, 1]
  plot_data[plot_data$outlier == FALSE, 3] <- NA

  boundaries <- t(data.frame(
    median(x, na.rm = TRUE) - estimates[[2]],
    median(x, na.rm = TRUE) + estimates[[2]],
    median(x, na.rm = TRUE) - 2 * estimates[[2]],
    median(x, na.rm = TRUE) + 2 * estimates[[2]]
  ))
  rownames(boundaries) <- c("-1s", "+1s", "-2s", "+2s")

  labels <- data.frame(
    x = c(10, 10, 10),
    y = c(boundaries[3, ][[1]], median(x, na.rm = TRUE), boundaries[4, ][[1]]),
    label = c(
      paste0("median - 2s: ", round(boundaries[3, ][[1]], 3)),
      paste0("median: ", median(x, na.rm = TRUE)),
      paste0("median + 2s: ", round(boundaries[4, ][[1]], 3))
    )
  )

  ggplot(plot_data) +
    geom_hline(aes(yintercept = median(x, na.rm = TRUE)), 
               linetype = "longdash", size = .5, alpha = 1 / 2) +
    # geom_hline(aes(yintercept = boundaries[1, ][[1]]), linetype = "twodash", size = .5, alpha = 1/2) +
    # geom_hline(aes(yintercept = boundaries[2, ][[1]]), linetype = "twodash", size = .5, alpha = 1/2) +
    geom_hline(aes(yintercept = boundaries[3, ][[1]]), 
               linetype = "dotted", size = .5, alpha = 1 / 2) +
    geom_hline(aes(yintercept = boundaries[4, ][[1]]), 
               linetype = "dotted", size = .5, alpha = 1 / 2) +
    geom_point(aes(index, outlier), size = 6, color = "coral") +
    geom_point(aes(index, x), size = 4, color = "cornflowerblue") +
    theme_light() +
    labs(
      x = "Index of observation",
      y = "Value of observation",
      caption = "Labels are the actual values of outlier data points"
    ) +
    geom_label(aes(index, outlier, label = x)) +
    geom_label(data = labels, aes(x, y, label = label), fontface = "bold")
}
MAD_plot(x)
