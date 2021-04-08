library(fpc)
library(tidyverse)
library(tibble)
dataset <- tibble(read.delim("data/MNdat525.txt", sep = "\t"))
kbest.p <- 3
results <- tibble()
for (i in 1:5) {
  d <- clusterboot(dataset, clustermethod = hclustCBI, method = "ward.D", k = kbest.p, B = 50, bootmethod = "boot", cut = "number", dissolution = 0.5, recover = 0.75, scaling = FALSE)
  d2 <- clusterboot(dataset, clustermethod = hclustCBI, method = "ward.D2", k = kbest.p, B = 50, bootmethod = "boot", cut = "number", dissolution = 0.5, recover = 0.75, scaling = FALSE)
  actual <- tibble(
    i = rep(i, length(d$bootmean)),
    "ward.D" = d$bootmean,
    "ward.D2" = d2$bootmean
  )
  results <- bind_rows(results, actual)
}
results <- results %>%
  pivot_longer(2:3,
    names_to = "method",
    values_to = "stability"
  )

p <- ggplot(results) +
  geom_boxplot(aes(i, stability, group = i)) +
  facet_wrap(~method) +
  labs(
    x = "iterations [1..5]",
    y = "stability score"
  )
ggsave("stability.pnd", p, device = "png", dpi = 300, width = 10, height = 6)
