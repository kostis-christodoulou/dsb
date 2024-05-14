library(tidyverse)
library(vroom)
library(showtext)
library(ggtext)

font_add_google("Montserrat", "Montserrat")
font_add_google("Ubuntu", "Ubuntu")
font_add_google("Oswald", "Oswald")
font_add_google("Lato", "Lato")

## Automatically use showtext to render text for future devices
showtext_auto()

# source of data https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-08-13
url <- "https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-08-13/emperors.csv"
emperors <- vroom(url)

glimpse(emperors)

# Plot 1
emperors %>%
  count(cause) %>%
  ggplot(aes(x = n, y = cause)) +
  geom_col() +
  geom_text(
    aes(label = n, x = n - .25),
    colour = "white",
    size = 5,
    hjust = 1
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  labs(
    title = "Cause of death of Roman Emperors",
    x= "number of emperors")+
  NULL

# Plot 2
# Arrange them in descending order, change font, 
# make sure title is top-left aligned

emperors %>%
  count(cause) %>%
  arrange(n) %>% 
  
  #arrange cause, using fct_inorder()
  mutate(cause = fct_inorder(cause)) %>% 
  ggplot(aes(x = n, y = cause)) +
  geom_col() +
  geom_text(
    aes(label = n, x = n - .25),
    colour = "white",
    size = 5,
    hjust = 1,
    family="Lato" # change font; if you havent installed extra fonts, comment this line out
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  theme(text=element_text(size=16, family="Lato"))+
  labs(
    title = "Cause of death of Roman Emperors",
    x= "number of emperors")+
  
  # ensure title is top-left aligned
  theme(plot.title.position = "plot")+
  NULL


# Plot 3: Highlight assassinations/executions with a different colour/fill
# define a new variable 'assassination' which we will use to colour differently
emperors_assassinated <- emperors %>%
  count(cause) %>%
  arrange(n) %>% 
  mutate(
    assassination = ifelse(cause == "Assassination" | cause == "Execution", TRUE, FALSE),
    cause = fct_inorder(cause)
  )

#define colours to use: grey for everything, tomato for assassination
my_colours <- c("grey80", "tomato")

#if you want to use hex codes, gplots::col2hex("colour name") will give you hex code
# grey70 = #B3B3B3, tomato = #FF6347

# let us create a text label to add as annotation to our graph
label <- "Execution is a third as likely \n as assassination"


emperors_assassinated %>%
  ggplot(aes(x = n, y = cause, fill = assassination)) +
  geom_col() +
  scale_fill_manual(values = my_colours)+
  geom_text(
    aes(label = n, x = n - .25),
    colour = "white",
    size = 5,
    hjust = 1,
    family="Lato"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  theme(text=element_text(size=12, family="Lato"))+
  theme(plot.title.position = "plot")+
  
  # change font colour 
  labs(colour = "assassination",
       title = "<b> Cause of death of Roman emperors</b><br>
       <span style = 'font-size:12pt'>Roughly half of the emperors died of <span style='color:#FF6347'>assassination</span> and <span style='color:#FF6347'>execution </span>.</span>",
       x= "Number of emperors",
       y = "")+
  
  
  #add a curve to draw attention to a value
  geom_curve(
    data = data.frame(x = 15, y = 3.2, xend = 8.1, yend = 5),
    mapping = aes(x = x, y = y, xend = xend, yend = yend),
    colour = "grey15",
    size = 0.5,
    curvature = 0.25,
    arrow = arrow(length = unit(2, "mm"), type = "closed"),
    inherit.aes = FALSE
  ) +
  
  # add the text label on the graph
  geom_text(
    data = data.frame(x = 15, y = 3, label = label),
    aes(x = x, y = y, label = label),
    colour="#FF6347",
    family="Lato",
    hjust = 0.5,
    lineheight = .8,
    inherit.aes = FALSE,
  ) +
  
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size=16),
    axis.title.y = element_text(angle = 0, vjust = 0.5,size=14),
    axis.text = element_text(size=12),
    legend.position = "none") +
  NULL