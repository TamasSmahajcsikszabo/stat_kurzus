library(vegan)
library(pracma)
library(tidyverse)

dataset <- read_csv("../../data/data.csv")
dataset <- dataset[c("PTelj", "PBoldog", "PMagány")]

pca <- rda(dataset, scale = TRUE)
loading <- scores(pca, choices = c(1, 2, 3))$species
rotated_loading <- varimax(loading)$loadings
iloading <- t(pinv(rotated_loading))
scores <- scale(dataset) %*% iloading


weight_matrix <- cor(scale(dataset), scores)


write.table(weight_matrix, "output/pca_weights.txt", row.names = FALSE, col.names = FALSE)
weights <- read.table("output/pca_weights.txt")

weight_content <- ""
for (r in 1:3) {
  actual_row <- paste0(unname(unlist(weights[r, ])), collapse = "\t")
  weight_content <- paste0(weight_content, actual_row, "\n")
}

line1 <- "3 (Number of variables)\n"
line2 <- "PTelj\n"
line3 <- "PBoldog\n"
line4 <- "PMagány\n"

file_content <- paste0(line1, line2, line3, line4, weight_content)
writeLines(file_content, con = "output/floadingbetolt.txt")
