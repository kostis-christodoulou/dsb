library(tidyverse)
library(janitor)
library(plotly)
library(ggiraph)

# https://www.who.int/data/gho/data/themes/topics/health-workforce
# Global Health Workforce statistics database 

nurses <- read_csv("nurses.csv") |> 
  clean_names()|> 
  filter(indicator == "Nursing and midwifery personnel (per 10,000)") |> 
  select(c(indicator,
           parent_location,
           spatial_dim_value_code,
           location,
           period,
           fact_value_numeric))





doctors <- read_csv("doctors.csv")|> 
  clean_names() |> 
  filter(indicator == "Medical doctors (per 10,000)")|> 
  select(c(indicator,
           parent_location,
           spatial_dim_value_code,
           location,
           period,
           fact_value_numeric
      ))


fubar <- bind_rows(
  doctors, nurses
)


plot1 <- fubar |> 
  ggplot()+
  aes(y=fact_value_numeric, x=period, colour=location)+
  geom_point(alpha = 0.3)+
  facet_grid(indicator~parent_location, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")


ggplotly(plot1,
         tooltip = c("location", "fact_value_numeric"))


plot2 <- fubar |> 
  ggplot()+
  aes(y=fact_value_numeric, x=period, colour=location, tooltip = location)+
  geom_point_interactive(alpha = 0.3)+
  facet_grid_interactive(indicator~parent_location, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")

girafe(ggobj = plot2)

