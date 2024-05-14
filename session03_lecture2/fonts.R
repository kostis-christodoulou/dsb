library(tidyverse)
library(showtext)
library(patchwork)

## Loading Google fonts (https://fonts.google.com/)
font_add_google("Montserrat", "Montserrat")
font_add_google("Ubuntu", "Ubuntu")
font_add_google("Oswald", "Oswald")
font_add_google("Rock Salt", "Rock Salt")

## Automatically use showtext to render text for future devices
showtext_auto()

## Tell showtext the resolution of the device,
## only needed for bitmap graphics. Default is 96
showtext_opts(dpi = 96)


plot <- 
  ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point() +
  labs(
    title = "Fuel Efficiency of 32 Cars",
    x= "Weight (x1000 lb)",
    y = "Miles per Gallon"
  )+
  theme_minimal()+
  NULL

p1 <- plot + theme(text=element_text(size=16, family="Montserrat"))
p2 <- plot + theme(text=element_text(size=16, family="Ubuntu"))
p3 <- plot + theme(text=element_text(size=16, family="Oswald"))
p4 <- plot + theme(text=element_text(size=16, family="Rock Salt"))


# Use patchwork to arrange  4 graphs on same page
(p1 + p2) / (p3 + p4)