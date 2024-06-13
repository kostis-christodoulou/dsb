# International trade in goods - Imports 2001-2018
# http://www.intracen.org/itc/market-info-tools/statistics-import-product-country/ 
# code for wine is 22-04

## Wine importing countries

library(gganimate)
library(ggthemes)
library(tidyverse)
library(readxl)
library(here)
library(viridis)
theme_set(theme_bw())


wine_imports <-  read_excel(here::here("data", "wineImports.xlsx"), skip = 13,
                            progress = readxl_progress(), .name_repair = "unique")

wine_exports <-  read_excel(here::here("data", "wineExports.xlsx"), skip = 13,
                            progress = readxl_progress(), .name_repair = "unique")


## tidying data into long format
wine_imports_long <- wine_imports %>%
  pivot_longer(cols = 2:19,
               names_to = "Year",
               values_to = "Value") %>%
  # turn Year to a factor variable
  mutate(Year = factor(Year)) %>% 
  
  group_by(Year) %>%
  mutate(rank = min_rank(-Value) * 1) %>%
  ungroup() %>%
  filter(rank <= 10)

wine_exports_long <- wine_exports %>%
  pivot_longer(cols = 2:19,
               names_to = "Year",
               values_to = "Value") %>%
  # turn Year to a factor variable
  mutate(Year = factor(Year)) %>% 
  group_by(Year) %>%
  mutate(rank = min_rank(-Value) * 1) %>%
  ungroup() %>%
  filter(rank <= 10)


## Animated Visualization of Imports
wine_imports_plot <- ggplot(wine_imports_long, aes(rank, group = Importers, 
                                   fill = Importers, color = Importers)) +
  geom_tile(aes(y = Value/2,
                height = Value,
                width = 0.9)) +
  geom_text(aes(y = 0, label = paste(Importers, " ")), vjust = 0.2, hjust = 1) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_reverse() +

  scale_fill_viridis(option = "D", discrete = TRUE)+
  scale_colour_viridis(option = "D", discrete = TRUE)+
  
  guides(colour = FALSE, fill = FALSE) +
  labs(title='{closest_state}', 
       x = "", y = "Wine Imports (in 000s US$)",
       caption = "Source: Trade Map, International Trade Centre, https://marketanalysis.intracen.org")+
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  
        axis.text.y  = element_blank(),  
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(Year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(wine_imports_plot, fps = 25, duration = 15, width = 800, height = 600)

## Animated Visualization of Exports

wine_exports_plot <- ggplot(wine_exports_long, aes(rank, group = Exporters, 
                                              fill = Exporters, color = Exporters)) +
  geom_tile(aes(y = Value/2,
                height = Value,
                width = 0.9)) +
  geom_text(aes(y = 0, label = paste(Exporters, " ")), vjust = 0.2, hjust = 1) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_reverse() +
  scale_fill_viridis(option = "D", discrete = TRUE)+
  scale_colour_viridis(option = "D", discrete = TRUE)+
  
  guides(colour = FALSE, fill = FALSE) +
  labs(title='{closest_state}', 
       x = "", y = "Wine Exports (in 000s US$)",caption = "Source: Trade Map, International Trade Centre, https://marketanalysis.intracen.org")+
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  
        axis.text.y  = element_blank(),  
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(Year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(wine_exports_plot, fps = 25, duration = 15, width = 800, height = 600)


