
library(tidyverse)
library(lubridate)

bikes <- readr::read_csv(
  here::here("data", "london_bikes.csv"))

bikes <- bikes %>%   
  mutate(
    wday = wday(date, label = TRUE),
    month = month(date),
    month_name=month(date, label = TRUE)) 

# generate new variable season_name to turn seasons from numbers to Winter, Spring, etc
bikes <- bikes %>%  
  mutate(
    season_name = case_when(
      month_name %in%  c("Dec", "Jan", "Feb")  ~ "Winter",
      month_name %in%  c("Mar", "Apr", "May")  ~ "Spring",
      month_name %in%  c("Jun", "Jul", "Aug")  ~ "Summer",
      month_name %in%  c("Sep", "Oct", "Nov")  ~ "Autumn",
    ),
    season_name = factor(season_name, 
                         levels = c("Winter", "Spring", "Summer", "Autumn")),
    weekend = case_when(
      wday %in%  c("Sat", "Sun")  ~ "Weekend",
      TRUE  ~ "Weekday",
    )
  )

                  
ggplot(data = bikes) # This just gives a blank canvas, as we haven't defined anything yet.
                  
# Plot axes, but we need a geometry
ggplot(data = bikes) +
  aes(x = mean_temp, y = bikes_hired)


# `aes()` outside as component
ggplot(data = bikes) +
  aes(x = mean_temp, y = bikes_hired)

# `aes()` inside, explicit matching
ggplot(data = bikes, 
       mapping = aes(x = mean_temp, y = bikes_hired))

# `aes()` inside, implicit matching
ggplot(bikes, 
       aes(mean_temp, bikes_hired))

# `aes()` inside, mixed matching
ggplot(bikes, 
       aes(y = bikes_hired, x =  mean_temp))


# Geometries
ggplot(
    bikes,
    aes(x = mean_temp, y = bikes_hired)
  ) +
  geom_point()

## Visual Properties of Layers
ggplot(
    bikes,
    aes(x = mean_temp, y = bikes_hired)
  ) +
  geom_point(
    colour = "#5989e3", #colour as a HEX code
    alpha = .25,
    shape = "X",
    stroke = 1,
    size = 4
  )


## `Setting` vs `Mapping` of Visual Properties
# # Setting colour to a fixed value, hex code = #5989e3
ggplot(
    bikes,
    aes(x = mean_temp, y = bikes_hired)
  ) +
  geom_point(
    colour = "#5989e3",
    alpha = .25
  )

# Mapping colour to a variable, season_name
ggplot(
    bikes,
    aes(x = mean_temp, y = bikes_hired)
  ) +
  geom_point(
    aes(colour = season_name),
    alpha = .25
  )


## Mapping Expressions
ggplot(
    bikes,
    aes(x = mean_temp, y = bikes_hired)
  ) +
  geom_point(
    aes(colour = mean_temp > 20),
    alpha = .25
  )

## Setting a Constant Property
ggplot(
    bikes,
    aes(x = date, y = bikes_hired)
  ) +
  geom_point(
    aes(colour = precipitation > 0,
        size = bikes_hired),
    shape = 18,
    alpha = .25
  )


## Setting a Constant Property
ggplot(
  bikes,
  aes(x = date, y = bikes_hired)
) +
  geom_point(
    aes(colour = precipitation > 0,
        size = bikes_hired
    ),
    shape = 23,
    colour = "black", # outside the aes()
    alpha = .25
  )

## Local vs. Global Encoding
# local encoding - colour = season_name applies only to geom_point()
ggplot(
  bikes,
  aes(x = mean_temp, y = bikes_hired)
) +
  geom_point(
    aes(colour = season_name),
    alpha = .25
  ) 

# Global encoding.  season_name applies to all geometries
ggplot(
  bikes,
  aes(x = mean_temp, y = bikes_hired,
      colour = season_name)
) +
  geom_point(
    alpha = .25
  )

## Global Colour Aesthetic
ggplot(
  bikes,
  aes(x = mean_temp, y = bikes_hired,
      colour = season_name)
) +
  geom_point(
    alpha = .25
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  )


## Overwrite Global Aesthetics
ggplot(
  bikes,
  aes(x = mean_temp, y = bikes_hired,
      colour = season_name,
      group = season_name)
) +
  geom_point(
    alpha = .25
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black"
  )

## Store a ggplot as an object
base_graph <-
  ggplot(
    bikes,
    aes(x = mean_temp, y = bikes_hired,
        colour = season_name,
        group = season_name)
  ) +
  geom_point(
    alpha = .25
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  )


class(base_graph)

## Inspect a ggplot Object
base_graph$data
base_graph$mapping


## Extend a ggplot Object: Add facets
base_graph + 
  facet_wrap(~season_name)

## Extend a ggplot Object: Add Layers
base_graph +
  geom_rug(
    alpha = .2

## Extend a ggplot Object: Add Labels
base_graph +
  labs(
    title = "TfL bike rentals vs temperature",
    x = "Mean temperature (°C)",
    y = "Total bike rentals"
  )

## Extend a ggplot Object: Add Labels
base_graph <- base_graph +
  labs(
    title = "TfL bike rentals vs temperature",
    x = "Mean temperature (°C)",
    y = "Total bike rentals",
    colour = "Season:"
  )

base_graph

## Extend a ggplot Object: No labels in x-axis, either x="" or x = NULL
base_graph +
  labs(
    x = "",
    caption = "Data: TfL"
  )

base_graph +
  labs(
    x = NULL,
    caption = "Data: TfL"
  )

  
## Extend a ggplot Object: Themes
  
base_graph + 
  theme_light()

base_graph + 
  theme_minimal()

base_graph + 
  theme_bw()

## Use pre-defined themes from {ggthemes}
library(ggthemes)
base_graph + 
  ggthemes::theme_economist()

base_graph + 
  ggthemes::theme_solarized()

  
## Set a Theme Globally
  
  ```{r}
theme_set(theme_light(base_size=18))

base_graph


## Save the Graphic
ggsave(base_graph, filename = "my_plot.png")
ggsave("my_plot.png") # saves last ggplot
ggsave("my_plot.png", 
       width = 8, height = 5, dpi = 600) #default in inches
ggsave("my_plot.pdf", 
       width = 20, height = 12, unit = "cm", device = cairo_pdf)

grDevices::cairo_pdf("my_plot.pdf", 
                     width = 10, height = 7)
base_graph
dev.off()

# IMDB movie data

movies <- readr::read_csv(here::here("data", "movies.csv"))

## `glimpse(movies)`
glimpse(movies)

## Drawing numerical values as bars
ggplot(movies, aes(x = title, y = gross)) +
  geom_col()

## Flipping axes
ggplot(movies, aes(y = title, x = gross)) +
  geom_col()

# alternatively, use coord_flip()
ggplot(movies, aes(x = title, y = gross)) +
  geom_col() +
  coord_flip()

## Choosing top/bottom 20 movies
# slice_max() How are films sorted?
movies %>% 
  slice_max(order_by = gross, n=20) %>% 
  ggplot(aes(x = gross, y = title)) +
  geom_col() +
  theme_minimal(base_size=6)+
  scale_x_continuous(labels = scales::label_dollar())

# slice_min() How are films sorted?
movies %>% 
  slice_min(order_by = gross, n=20) %>% 
  ggplot(aes(x = gross, y = title)) +
  geom_col() +
  theme_minimal(base_size=6)+
  scale_x_continuous(labels = scales::label_dollar())

## `forcats::fct_reorder()`
movies %>% 
  slice_max(order_by = gross, n=20) %>% 
  mutate(title = fct_reorder(title, gross)) %>% 
  ggplot(aes(x = gross, y = title)) +
  geom_col() +
  theme_minimal(base_size = 6)+
  labs(
    title = "",
    subtitle = "",
    x = "Gross earnings (US$)",
    y = NULL
  ) +
  NULL

## Create the following from the IMDB `movies` data
movies %>% 
  slice_max(order_by = gross, n=20) %>% 
  mutate(title = fct_reorder(title, gross)) %>%  
  ggplot(aes(x = gross, y = title, fill = genre)) +
  geom_col() +
  theme_minimal(base_size = 6)+
  labs(
    title = "Action and Adventure movies dominate the box office",
    subtitle = "Gross Earnings (US$) at the box office, sample of 3000 IMDB movies",
    x = NULL,
    y = NULL,
    fill = "Movie Genre"
  )+
  scale_x_continuous(labels = scales::label_dollar())+
  # ensure title is top-left aligned
  theme(plot.title.position = "plot")

## Reordering based on frequency `forcats::fct_infreq()`
movies %>% 
  mutate(genre = fct_infreq(genre)) %>%
  ggplot(aes(y=genre))+
  geom_bar()+
  theme_minimal(base_size=6)

# to reverse the order, use fct_rev()...
movies %>% 
  mutate(genre = fct_rev(fct_infreq(genre))) %>%
  ggplot(aes(y=genre))+
  geom_bar()+
  theme_minimal(base_size=6)

## Reordering based on numerical values `forcats::fct_reorder()`
library(gapminder)

gapminder %>%
  filter(continent == "Europe") %>%
  mutate(
    country = fct_reorder(country, lifeExp, median)
  ) %>%
  ggplot(aes(year, country, fill = lifeExp)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  theme_minimal(base_size=6)

gapminder %>%
  filter(continent == "Europe") %>%
  mutate(country = fct_rev(fct_reorder(country, lifeExp, median))) %>%
  ggplot(aes(year, lifeExp)) + 
  geom_line() +
  facet_wrap(vars(country))+
  theme_minimal(base_size=6)+
  labs(
    title = "Life Expectancy in Europe, 1952 - 2007",
    x = NULL,
    y = NULL,
    caption = "Data Source: Gapminder Project"
  )

## `ggThemeAssist` add-in
https://github.com/calligross/ggthemeassist

