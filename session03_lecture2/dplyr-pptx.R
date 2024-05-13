# Load libraries  ----
library(tidyverse)
library(officer)
library(nycflights13)

# Some data manipulation using dplyr
departure_arrival_delays_table <- flights %>%
  group_by(carrier) %>%
  summarise(
    mean_dep_delay    = mean(dep_delay, na.rm=TRUE),
    median_dep_delay    = median(dep_delay, na.rm=TRUE),
    mean_arr_delay    = mean(arr_delay, na.rm=TRUE),
    median_arr_delay    = median(arr_delay, na.rm=TRUE),
    n = n()
  ) %>% 
  arrange(desc(n)) %>% 
  left_join(airlines, by = "carrier") %>% 
  select(name, n, mean_dep_delay, mean_arr_delay)  



top_carriers_table <- flights %>% 
  count(carrier, sort=TRUE) %>% 
  mutate(percent = scales::number(100*n/sum(n), accuracy=0.01)) 

top_destinations_table <- flights %>% 
  count(dest, sort=TRUE) %>% 
  mutate(percent = scales::number(100*n/sum(n), accuracy=0.01)) %>% 

  left_join(airports, by = c("dest"= "faa")) %>% 
  
  # keep the first four columns only
  select(1:4) %>% 
  
  # keep the top 20 destinations only
  slice_max(order_by = percent,
            n = 20)




#  ggplot graph 
departure_arrival_delays_chart <- flights %>% 
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



# Make a PPT presentation

doc <- read_pptx()
doc <- add_slide(doc)
doc <- ph_with(doc, value = "Flights out of NYC Airports in 2013", location = ph_location_type(type = "title"))

doc <- ph_with(doc, value = top_carriers_table, location = ph_location_left())
doc <- ph_with(doc, value = top_destinations_table, location = ph_location_right())

# add table output
doc <- add_slide(doc)
doc <- ph_with(doc, value = departure_arrival_delays_table, location = ph_location_type(type = "body"))

# add ggplot
doc <- add_slide(doc)
doc <- ph_with(doc, value = departure_arrival_delays_chart, location = ph_location_type(type = "body"))

# save doc
print(doc, target = "delays_report.pptx")

