library(stringr)
source("../../src/functions.R")
dataset <- read_csv("../../data/data.csv")

colnameVector <- str_detect(colnames(dataset), pattern = "Diener")
ASED_matrix <- ASED_df(dataset[, colnameVector])
dist_matrix <- ASED_matrix
neighbour_matrix <- find_neighbours(dist_matrix, radius_vector = seq(0.1, 1.0, by = 0.05))
neighbour_summary <- find_neighbours(dist_matrix, radius_vector = seq(0.1, 1.0, by = 0.05), summarize = TRUE)
density_summary <- find_neighbours(dist_matrix, radius_vector = seq(0.1, 1.0, by = 0.05), summarize = TRUE, estimate_density = TRUE, preselection = TRUE)
