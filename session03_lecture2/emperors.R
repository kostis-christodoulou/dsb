library(tidyverse)
library(vroom)
library(extrafont)
library(ggtext)

loadfonts(device="pdf")

# source of data https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-08-13
url <- "https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-08-13/emperors.csv"
emperors <- vroom(url)

glimpse(emperors)

# Plot 1
emperors %>%
  
  # count causes of death
  count(cause) %>%
  ggplot(aes(x = n, y = cause)) +
  geom_col() +
  
  # add n to each bar
  geom_text(
    aes(label = n, x = n - .25),
    colour = "white",
    size = 5,
    hjust = 1
  ) +
  theme_minimal() +
  labs(
    title = "Cause of death of Roman Emperors",
    x= "Number of emperors",
    y = NULL) 

# Plot 2
# Arrange them in descending order, change font, 
# make sure title is top-left aligned


emperors %>%
  
  # count causes of death
  count(cause) %>%
  
  mutate(cause = fct_reorder(cause,n)) %>% 
  
  ggplot(aes(x = n, y = cause)) +
  geom_col() +
  
  # add n to each bar
  geom_text(
    aes(label = n, x = n - .25),
    colour = "white",
    size = 5,
    hjust = 1
  ) +
  theme_minimal() +
  theme(text=element_text(size=16))+ 
  labs(
    title = "Cause of death of Roman Emperors",
    x= "number of emperors",
    y = NULL)+
  
  # ensure title is top-left aligned
  theme(plot.title.position = "plot") 


  

# Plot 3: Highlight assassinations/executions with a different fill
# define a new variable 'assassination' which we will use to colour differently
emperors_assassinated <- emperors %>%
  count(cause) %>%
  mutate(
    assassination = ifelse(cause == "Assassination" | cause == "Execution", TRUE, FALSE),
    cause = fct_reorder(cause,n)
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
    colour = "white", size = 5, hjust = 1,
    family="Lato"
  ) +
  theme_minimal() +
  
  # change font colour 
  labs(colour = "assassination",
       title = "<b> Roughly half of Roman emperors died of <span style='color:#FF6347'>assassination</span> and <span style='color:#FF6347'>execution </span></b><br>
       <span style = 'font-size:12pt'>Cause of death of Roman emperors</span>",
       x= "Number of emperors",
       y = NULL)+
  
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
    text=element_text(size=12, family="Lato"),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size=16),
    axis.title.y = element_text(angle = 0, vjust = 0.5,size=14),
    axis.text = element_text(size=12),
    legend.position = "none",
    # remove gridlines
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    # remove numbers from x-axis
    axis.title.x = element_blank(),
    axis.text.x=element_blank())

