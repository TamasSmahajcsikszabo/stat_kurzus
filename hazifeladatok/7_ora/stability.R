results <- tibble()
for (i in seq(15, 2, -1)) {
  kbest.p <- i
  wardclust <- clusterboot(dataset_scaled, clustermethod = hclustCBI, method = "ward.D2", k = kbest.p)
  stab <- round(wardclust$bootmean, 4)
  dissolved <-wardclust$bootbrd
  solution <- tibble(
    stabilitás = stab,
    `feloszlások száma` = dissolved,
    k_total = rep(i,length(stab)),
    k = seq(1,length(stab))
  )
  results <- bind_rows(results, solution)
}