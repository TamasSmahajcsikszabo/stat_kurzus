# package declarations
library(ggplot2)
library(tibble)
library(readr)

# data import
dataset <- readr::read_delim("./hazifeladatok/1_ora/RDat.txt", delim="\t")
names(dataset) <- c("index", "ideges", "feszult")

# plot
ggplot(data=dataset, aes(x=ideges,y=feszult)) +
  geom_point(aes(alpha = interaction(ideges, feszult)),
             size=4,
             color="cornflowerblue", 
             show.legend = FALSE) +
  geom_jitter(alpha = 1/30, size = 3)+
  scale_x_continuous(breaks = seq(1,6)) +
  scale_y_continuous(breaks = seq(1,6)) +
  labs(
    title = "A megélt feszültség és az idegesség között a mintán lineáris együttjárás mutatkozik",
    x = "Ideges",
    y = "Feszült",
    caption = "Az adatpontok alatt random szóródással az egyes mintapontok megoszlása látszik"
  )+
  theme_light()
