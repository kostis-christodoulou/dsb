options(digits=3)
library(tidyverse)
library(lubridate)
library(here)
library(mosaic)
library(infer)
library(ggridges)
library(viridis)

# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/
movies <- read_csv(here::here('data', 'movies.csv'))

glimpse(movies)

movies %>% 
  count(genre,sort=TRUE)

#**************************************
#choose genres that have at least 5 movies
genres_to_choose <- movies %>%
  group_by(genre) %>% 
  summarise(count = n()) %>% 
  filter(count>5) %>% 
  select(genre) %>% 
  pull()


#**************************************
# generate a new variable, return on budget; namely for every $ spent on budget, 
# how many $ did it make at the box office (gross)
movies <- movies %>%
  mutate(return_on_budget = (gross/budget) - 1) 
  
#**************************************
# Plot boxplots of return on budget, faceted by genre
movies %>%
  filter(genre %in% genres_to_choose) %>%
  ggplot(aes(x = return_on_budget, colour=genre)) +
  geom_boxplot() +
  facet_wrap(~genre, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Distribution of return on budget by film genre")+
  NULL


# Plot ECDF of return on budget, faceted by genre
movies %>%
  filter(genre %in% genres_to_choose) %>%
  ggplot(aes(x = return_on_budget, colour=genre)) +
  stat_ecdf(geom = "step", pad = FALSE) +
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~genre, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Distribution of return on budget by film genre",
       x = "Film Rating",
       y = " ")+
  NULL


#**************************************
# Confidence Interval (CI) using the formula mean +- t_critical * SE

genre_formula_ci <- movies %>%
  filter(genre %in% genres_to_choose) %>% 
  group_by(genre) %>% 
  summarise(mean_rating = mean(rating),
            median_rating = median(rating),
            sd_rating = sd(rating),
            count = n(),
            # get t-critical value with (n-1) degrees of freedom
            t_critical = qt(0.975, count-1),
            se_rating = sd_rating/sqrt(count),
            margin_of_error = t_critical * se_rating,
            rating_low = mean_rating - margin_of_error,
            rating_high = mean_rating + margin_of_error
  ) %>% 
  arrange(desc(mean_rating))


genre_formula_ci
#**************************************
# Plot distributions of ratings, faceted by genre
movies %>%
  filter(genre %in% genres_to_choose) %>% 
  ggplot(aes(x = rating, colour=genre)) +
  geom_density() +
  facet_wrap(~genre)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Distribution of IMDB ratings by film genre",
       x = "Film Rating",
       y = " ")+
  NULL


# we will draw a violin plot and then use position="jitter" or geom_jitter() 
# to see how spread out the actual points are

# we can also superimpose  the means as a big orange dot,
# but first we must create a tibble that has two columns: genre and rating

mean_rating_tibble <- genre_formula_ci %>% 
  select(genre, mean_rating) %>% 
  rename(rating=mean_rating)

ratings_tibble <- movies %>%
  filter(genre %in% genres_to_choose)

movies %>%
  filter(genre %in% genres_to_choose) %>% 
  ggplot(aes(x = reorder(genre, rating), y = rating, colour=genre)) +
  geom_violin()+
  geom_point(position = "jitter", size = 0.8, alpha = 0.95) + 
  geom_point(data = mean_rating_tibble, colour = "orange", size = 5)+
  labs(x=" ",
       y= "Mean IMDB Rating", 
       title="Which film genres have the highest mean IMDB ratings?") + 
  coord_flip()+
  theme_bw()+
  labs(x="")+
  theme(legend.position = 'none')+
  NULL

#**************************************
# Confidence Interval (CI) using the formula mean +- t_critical * SE

#visualise CIs for all genres. 
ggplot(genre_formula_ci, aes(x=reorder(genre, mean_rating), y=mean_rating, colour=genre)) +
  geom_point() +
  geom_errorbar(width=.5, aes(ymin=rating_low, ymax=rating_high)) + 
  labs(x=" ",
       y= "Mean IMDB Rating", 
       title="Which film genres have the highest mean IMDB ratings?") + 
  theme_bw()+
  coord_flip()+
  theme(legend.position = "none")+
  NULL
