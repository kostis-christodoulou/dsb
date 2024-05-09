library(tidyverse)
library(gapminder)
library(nycflights13)
library(gganimate) # for animation
library(gifski) # needed to render GIFs to animation
library(plotly) # interactive charts

dplyr::filter(gapminder, 
              continent == "Asia")


dplyr::filter(gapminder, 
              continent == "Asia",
              year == "2007")

dplyr::select(gapminder, 
              year, country, gdpPercap)

dplyr::arrange(gapminder, 
               pop)

dplyr::arrange(gapminder, 
               desc(pop))
                  
# The pipe operator `%>%` or  `|>`
# - Starts with a dataframe, ends with a dataframe
# - It takes the dataframe  on the left-hand-side **AND THEN** passes it to right-hand-side function; literally, drops it in as the first argument.


## Countries with highest `lifeExp` in 2007

# do it in intermediate steps
gapminder_2007 <- filter(gapminder, year == 2007)
gapminder_2007 <- select(gapminder_2007, country, continent, lifeExp)
gapminder_2007 <- arrange(gapminder_2007, desc(lifeExp))
gapminder_2007

# ready for a headache?
arrange(select(filter(gapminder, year == 2007), 
               country, continent, lifeExp), 
        desc(lifeExp)) -> gapminder_2007
gapminder_2007

                    
## Read the pipe operator as <b style='color:#28a87d;'>*and then*... </b> 

gapminder %>% 
  filter(year == 2007) %>% 
  select(country, continent, lifeExp) %>% 
  arrange(desc(lifeExp))
                  

# RStudio keyboard shortcut for the pipe operator 
#   
#   - `Ctrl` + `Shift` + `M` (Windows) 
#   - `Cmd` + `Shift` + `M` (Mac).

## `summarise()`
gapminder %>% 
  summarise(total_GDP = sum(gdpPercap), 
            max_pop = max(pop),
            median_lifeExp = median(lifeExp))


## `n()` and `n_distinct()`
                  

gapminder %>% 
  summarise(count = n()) # No of rows in a dataset/group

gapminder %>% 
  summarise(count = n(), # No of rows in a dataset/group
            distinct_countries = n_distinct(country))

                    
## `group_by()` - Groups cases by common values of one or more columns.


gapminder %>% 
  group_by(continent)

gapminder %>% 
  group_by(continent, year)

## `group_by()` and `summarise()`

gapminder %>% 
  group_by(continent) %>% 
  summarise(mean_lifexp = mean(lifeExp))

gapminder %>% 
  group_by(continent, year) %>% 
  summarise(mean_lifexp = mean(lifeExp))

## Animation

ggplot(gapminder) + 
  aes(x = gdpPercap, y = lifeExp, 
      size = pop, 
      colour = country) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  # Switch to log scale; use $ units
  scale_x_log10(labels = scales::dollar) + 
  facet_wrap(~continent) +
  theme_bw()+
  
  # Here comes the gganimate specific code
  labs(title = 'Year: {frame_time}', 
       x = 'GDP per capita', 
       y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# -------------------------------
                  
## `nycflights13` data package
library(nycflights13) 
library(skimr)
glimpse(flights)

## `dplyr::count()` - Quickly count unique values of one or more variables. 
# Number of flights departing from each `origin` airport

flights %>% 
  
  # count() is the same as group_by()... summarise() 
  # `sort=TRUE` gives table in descending order
  count(origin, sort = TRUE) %>% 
  
  # mutate() generates a new column called `prop` which is the proportion of flights 
  # calculated as number of flights `n` divided by the `sum(n)`
  mutate(prop = n/sum(n))

## What was the longest `arr_delay`?

flights %>% 
  arrange(desc(arr_delay))

## What was the average `arr_delay` for a `carrier`, say `UA`? 
# 
# - First we need to identify the relevant data we're going to analyse.
# - Next we need to **choose** only those flights which are `UA` United flights.
# - Finally, we need to calculate the average value, or **mean**, of `arr_delay`.
# - If there are any `NA`s, the result will be `NA`, so use `mean(arr_delay, na.rm = TRUE)`


flights %>% 
  
  # just choose UA, or United, flights
  filter(carrier == "UA") %>%
  
  # `na.rm = TRUE` will disregrard any missing values (NA)
  summarise(mean_arrival_delay = mean(arr_delay,na.rm = TRUE))


## Mean `dep_delay` and `arr_delay` for all airlines

flights %>% 
  group_by(carrier) %>%
  summarise(
    count = n(),
    mean_dep_delay = mean(dep_delay, na.rm = TRUE),
    mean_arrival_delay = mean(arr_delay,na.rm = TRUE)) %>% 
  arrange(desc(count))

## Saving transformed data (`<-`)

# **Prints**, but does not store, resulting tibble
flights %>% 
  filter(carrier == "UA")

# **Saves** resulting tibble called `UA_flights`
UA_flights <- flights %>% 
  filter(carrier == "UA")

# **Prints and Saves** resulting tibble

(
  UA_flights <- flights %>% 
  filter(carrier == "UA")
)

# dplyr --> ggplot

arrival_delay_chart <- flights %>% 
  # Group by tailnum and calculate summary statistics
  group_by(tailnum) %>%
  summarise(
    mean_dep_delay = mean(dep_delay),
    mean_arr_delay = mean(arr_delay),
    n = n()) %>%
  
  # Filter for planes with more than 100 flights
  filter(n > 100) %>% 
  
  # Sort by mean arrival delay in descending order
  arrange(desc(mean_arr_delay)) %>%
  
  # Pass resulting dataframe to ggplot() to create the scatter plot
  ggplot()+
  aes(x = mean_dep_delay, y = mean_arr_delay, size=n) +
  geom_point(alpha=0.20) +
  geom_abline(intercept = 0, slope = 1, size = 1.5, colour="red") +
  labs(
    title = "Most planes manage to make up time even if they depart late",
    x = "Mean departure delay (minutes)",
    y = "Mean arrival delay (minutes)",
    size = 'Number of flights'
  )+
  
  theme_bw()+
  NULL

arrival_delay_chart

# make it interacgtive by passing chart to ggplotly()

ggplotly(arrival_delay_chart)

