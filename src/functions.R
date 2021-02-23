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
    print(paste0("Run ", runname, " with ", variable))

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
