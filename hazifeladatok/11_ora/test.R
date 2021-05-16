library(mclust)
d <- read.table("MBedat.txt", header = TRUE)

set.seed(242)
mcl <- Mclust(d, G = 1:11)


plot(mcl, "BIC", legendArgs = list(x = "bottomright", ncol = 5))
