---
title: "Házi feladatok megoldása 12."
subtitle: "Különböző klasszifikációk összehasonlítása Centroid és Exacon módszerrel ROPstatban"
csl: apa7.csl
output:
  pdf_document: default
  html_document:
    df_print: paged
  word_document: default
---

Smahajcsik-Szabó Tamás, M9IJYM



```{r echo=FALSE}
# importing C++ code, R libraries and data
options(warn=-1)
#setwd('/home/tamas/repos/stat_kurzus/hazifeladatok/7_ora')
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
suppressMessages(source("../../src/functions.R"))
#suppressMessages(source("../../src/mad.R"))
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
suppressMessages(table1 <- read_delim("output/12_1.csv", delim=";"))
suppressMessages(table2_kka_centroids <- read_delim("output/12_2.csv", delim=";"))
suppressMessages(table2_kka_mka_centroids <- read_delim("output/12_2_2.csv", delim=";"))
suppressMessages(table2_kka_mka_pairs <- read_delim("output/12_2_3.csv", delim=";"))
suppressMessages(table2_kka_mka_frequencies <- read_delim("output/12_2_4.csv", delim=";"))
suppressMessages(table3_kka_mka_rate <- read_delim("output/12_3_1.csv", delim=";"))
suppressMessages(table3_kka_mka_exacon <- read_delim("output/12_3_2.csv", delim=";"))
suppressMessages(table3_kka_mka_tpyes <- read_delim("output/12_3_3.csv", delim=";"))
suppressMessages(table4_gender_desc <- read_delim("output/12_4_1.csv", delim=";"))
suppressMessages(table4_gender_comparison <- read_delim("output/12_4_2.csv", delim=";"))
suppressMessages(table4_gender_comparison2 <- read_delim("output/12_4_3.csv", delim=";"))
suppressMessages(table5 <- read_delim("output/12_5.csv", delim=";"))

```

## 1. A 10. óra 6. feladatában elmentett BIC-megoldás klaszterváltozójával végezz centroid-elemzést ROPstatban! Mely klaszterek vannak a legközelebb egymáshoz? Találsz itt összevonandó klasztereket?

```{r echo=FALSE}
knitr::kable(table1, "simple")
```

# # 2. Hasonlítsd össze a 8. óra 1. feladatának legjobb k-közép megoldását és a 10. óra 6. feladatában elmentett BIC-megoldás klaszterváltozóját a ROPstat centroid-elemzésével! Hány klaszterpár tekinthető igen közelinek?

```{r echo=FALSE}
knitr::kable(table2_kka_centroids, "simple")

knitr::kable(table2_kka_mka_centroids, "simple")

knitr::kable(table2_kka_mka_pairs, "simple")

knitr::kable(table2_kka_mka_frequencies, "simple")
```

## 3. Hasonlítsd össze a 8. óra 1. feladatának legjobb k-közép megoldását és a 10. óra 6. feladatában elmentett BIC-megoldás klaszterváltozóját a ROPstat EXACON modulja segítségével! Mennyire tekinthető hasonlónak a két megoldás a Jaccard és a korrigált Rand index szerint?

```{r echo=FALSE}
knitr::kable(table3_kka_mka_rate, "simple")

knitr::kable(table3_kka_mka_exacon, "simple")

knitr::kable(table3_kka_mka_tpyes, "simple")
```

## 4. Hasonlítsd össze a férfiak és a nők almintáját a 8. óra 1. feladatának legjobb k-közép megoldása segítségével! Melyik mintázatú klaszterben a legkisebb, illetve legnagyobb a nők aránya? Tudnál erre szakmai magyarázatot adni?

```{r echo=FALSE}
knitr::kable(table4_gender_desc, "simple")

knitr::kable(table4_gender_comparison, "simple")

knitr::kable(table4_gender_comparison2, "simple")
```

## 5. Melyik mintázatú klaszterben a legkisebb, illetve legnagyobb a 60 év felettiek aránya? Tudnál erre szakmai magyarázatot adni?

```{r echo=FALSE}
kka<- table5 %>% 
    mutate(age60 = if_else(Age >= 60, 1, 0)) %>%
    group_by(KKA7) %>%
    summarise(n = round(sum(age60)/n(), 3))

age60 <-table5 %>%
    mutate(age60 = if_else(Age >= 60, 1, 0))
write_csv(age60, "age60.csv")


mka <- table5 %>% 
    mutate(age60 = if_else(Age >= 60, 1, 0)) %>%
    group_by(MKA3) %>%
    summarise(n = round(sum(age60)/n(), 3))

```

```{r echo=FALSE}
knitr::kable(kka, "simple")
```
```{r echo=FALSE}
knitr::kable(mka, "simple")
```

