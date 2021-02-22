library(readr)
library(WRS)
library(Rcpp)
sourceCpp("functions.cpp")

dataset <- read_csv("../data/data.csv")
x <- dataset$PBoldog
y <- dataset$PEgészs
CD(x, y)

get_tau <- function(x, y, algorithm = "Kendall") {
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
  tau
}

get_tau(x, y) == tau(x, y)[1]
