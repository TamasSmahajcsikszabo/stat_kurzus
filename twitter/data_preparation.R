library(tidyverse)
library(caret)
source("twitter/functions.R")
all_data <- get_lines("twitter/data/")

filtered_data <- all_data %>% 
  distinct(status_id, .keep_all = TRUE)


texts <- tibble(
  text = filtered_data$text
) %>% 
  mutate(id = row_number()) %>% 
  dplyr::select(id, text)

write_csv(texts, "twitter/texts.csv")

id_table <- filtered_data %>%  
  mutate(id = row_number())
saveRDS(id_table, "twitter/id_table.RDS")

#liwc_results <- read_csv("twitter/liwc_results.csv")
#liwc_results <- liwc_results[-1,]
#colnames(liwc_results)[1:2] <- c("id", "text")
#write_csv(liwc_results, "twitter/liwc_results.csv")


# LIWC results refinements
liwc_results <- read_csv("twitter/liwc_results.csv")

replace <- function(x) {
  gsub(",", ".", x)
}
liwc_results <- map_df(liwc_results[,-c(1:10)], replace)
liwc_results <- map_df(liwc_results, as.numeric)

threshold <- 0.7
cormatrix <- cor(liwc_results)
mask <- data.frame(!cormatrix <= threshold)
rownames(mask) <- colnames(cormatrix)
colnames(mask) <- colnames(cormatrix)

correlations <- tibble()
for (i in 1:nrow(mask)) {
  for (j in 1:ncol(mask)) {
    if (!rownames(mask[i,]) == colnames(mask)[j]) {
      if (mask[i,j]) {
        actual = tibble(x = rownames(mask[i,]),
                        y = colnames(mask)[j])
        correlations <- bind_rows(correlations, actual)
      }
    }
  }
}

correlations <- correlations %>% 
  group_by(x,y) %>% 
  summarise(n = n())

liwc_results <- liwc_results[, !colnames(liwc_results) %in% c("verb", "pronoun", "ppron")]
plot(liwc_results[1:1000,1:10])
saveRDS(liwc_results, "twitter/liwc_results.RDS")

# OTHER varialbes
metadata <- tibble(
  id = 1:nrow(liwc_results)
)
metadata <- bind_cols(metadata, id_table[, c(78:82)])

mentions_length <- id_table %>% 
  mutate(mentions_length = length(mentions_user_id))