suppressMessages(library(readr))
suppressMessages(library(ggrepel))
suppressMessages(library(tidyverse))
suppressMessages(library(effsize))
suppressMessages(library(AICcmodavg))
suppressMessages(library(broom))
suppressMessages(library(confreq))
suppressMessages(library(stringr))
suppressMessages(library(kableExtra))
suppressMessages(library(caret))
suppressMessages(library(tidyverse))
suppressMessages(library(dendextend))
suppressMessages(library(ggdendro))
suppressMessages(library(stats))
suppressMessages(library(ClusterR))
suppressMessages(library(ggpubr))
suppressMessages(library(cluster))
suppressMessages(library(mclust))
suppressMessages(dataset <- read_csv("../../data/data.csv"))
source("../../src/functions.R")
dataset <- dataset[c("PTelj", "PBoldog", "PMagány")]


# 1.

mclust_fit10 <- autocluster(dataset, method = "mclust", k_range = 3:10)
saveRDS(mclust_fit10, "output/mclust_fit10.RDS")
# mclust_fit10["BIC plot"]
# mclust_fit10["boundary plot"]
# mclust_fit10["PCA plot"]
# mclust_fit10["homogenity"]
# mclust_fit10["best k"]
# mclust_fit10["density plot"]
# mclust_fit10["ICL plot"]

mclust_fit30 <- autocluster(dataset, method = "mclust", k_range = 3:30)
saveRDS(mclust_fit30, "output/mclust_fit30.RDS")
# mclust_fit30["BIC plot"]
# mclust_fit30["boundary plot"]
# mclust_fit30["PCA plot"]
# mclust_fit30["homogenity"]
# mclust_fit30["best k"]
# mclust_fit30["density plot"]
# mclust_fit30["ICL plot"]

mclust_fit3 <- autocluster(dataset, method = "mclust", k_range = 3)
saveRDS(mclust_fit3, "output/mclust_fit3.RDS")

get_color_scale(3)

icl_fit <- autocluster(dataset, method = "mclust", k_range = 1:10)
saveRDS(icl_fit, "output/icl_fit.RDS")
icl_fit_prior <- autocluster(dataset, method = "mclust", k_range = 1:10, prior = TRUE)
saveRDS(icl_fit_prior, "output/icl_fit_prior.RDS")
icl_fit_prior["ICL plot"]

dataset <- scale(dataset)
mkafit <- Mclust(dataset, G = 10)
saveRDS(mkafit, "output/G10fit.RDS")

mkafit <- Mclust(dataset, G = 3)
saveRDS(mkafit, "output/G3fit.RDS")
