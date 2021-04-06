library(tidyverse)
dataset = tibble::tibble(
  y = rnorm(10000),
  x = rnorm(10000))

xsum = summary(dataset$x)
ysum = summary(dataset$y)

dataset = dataset %>%
  mutate(g = if_else(x < xsum[2], "a", "b" ))


data("mtcars")
orig_blue <- "0035A1"
fire_engine = "#CE2029"
persian = "#cc3333"
zaphire="#0F52BA"
egyptian = "#1034A6"
blue = "#184998"
red = "#f74d27"
gold = "#f8d60b"
ggplot(dataset) +
  #geom_point(aes(mpg,disp, group=cyl), size=4, color="black", alpha=1/1, show.legend = FALSE) +
  geom_point(aes(x,y, group=y, color=factor(g)), size=4, alpha=1/1, show.legend = FALSE) +
  scale_color_manual(values=c(persian, zaphire, gold)) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

ggplot(dataset) +
  #geom_point(aes(mpg,disp, group=cyl), size=4, color="black", alpha=1/1, show.legend = FALSE) +
  geom_point(aes(x,y, group=y, color=factor(g)), size=4, alpha=1/1, show.legend = FALSE) +
  scale_color_manual(values=c(red, blue, gold)) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )
ggplot(dataset) +
  #geom_point(aes(mpg,disp, group=cyl), size=4, color="black", alpha=1/1, show.legend = FALSE) +
  geom_point(aes(x,y, group=y, color=factor(g)), size=16, alpha=1/1, show.legend = FALSE) +
  scale_color_manual(values=c(red, blue, "#cfb53b")) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )
