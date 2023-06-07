library(tidyverse)
library(lubridate)
library(ggmap)
library(ggrepel)
library(gridExtra)
library(pander)
library(here)
library(janitor)
library(skimr)
library(leaflet)
library(tmap)
library(tmaptools)
library(hrbrthemes)
library(mapview)
library(viridis)
library(sf)

#use sf::read_sf() to read in London Wards shapefile
london_wards_sf <- read_sf(here("data/London-wards-2018_ESRI/London_Ward.shp"))

glimpse(london_wards_sf)

# what type of geometry does our shapefile have?
st_geometry(london_wards_sf)

# transfrom CRS to 4326, or pairs of latitude/longitude numbers
london_wgs84 <-  london_wards_sf %>% 
  st_transform(4326) # transfrom CRS to WGS84, latitude/longitude


london_wgs84$geometry


sep22 <- read_csv(here("data/stop-search/2022-09-metropolitan-stop-and-search.csv")) %>% 
  clean_names() %>% 
  
  # rename longitude/latitude to lng/lat, as this is how they are used in leaflet
  rename(lng = longitude,
         lat = latitude) %>% 
  filter(lng <0.5)


# concentrate in top three searches, age_ranges, and officer defined ethnicities
which_searches <- c("Controlled drugs", "Offensive weapons","Stolen goods" )
which_ages <- c("10-17", "18-24","25-34", "over 34")
which_ethnicity <- c("White", "Black", "Asian")

sep22_offence <- sep22 %>% 
  
  # filter out stop-and-search where no further action was taken
  filter(outcome != "A no further action disposal") %>% 
  
  #filter out  rows with no latitude/longitude
  filter(!is.na(lng)) %>% 
  filter(!is.na(lat)) %>% 
  
  # concentrate in top searches, age_ranges, and officer defined ethnicities
  filter(object_of_search %in% which_searches) %>% 
  filter(age_range %in% which_ages) %>% 
  filter(officer_defined_ethnicity %in% which_ethnicity) %>% 
  
  # relevel factors so everything appears in correct order
  mutate(
    object_of_search = fct_relevel(object_of_search, 
                                   c("Controlled drugs", "Offensive weapons","Stolen goods")), 
    age_range = fct_relevel(age_range, 
                            c( "10-17","18-24", "25-34", "over 34")), 
    officer_defined_ethnicity = fct_relevel(officer_defined_ethnicity, 
                                            c("White", "Black", "Asian"))
  )

glimpse(sep22_offence)

# NB: make sure to transform to a  common CRS. 
# Here we retrieve and apply the CRS of london_wgs84 
sep22_offence_sf <-  st_as_sf(sep22_offence, 
                              coords=c('lng', 'lat'), 
                              crs=st_crs(london_wgs84))

# Alternatively, you can explicitly set the CRS to, e.g, 4326, or WGS84 
sep22_offence_sf <-  st_as_sf(sep22_offence, 
                              coords=c('lng', 'lat'), 
                              crs=4326)

glimpse(sep22_offence_sf)


ggplot() +
  # draw polygons from London wards shapefile
  geom_sf(data = london_wgs84, fill = "#3B454A", size = 0.125, colour = "#b2b2b277") +
  
  # add points from stop-and-search shapefile
  geom_sf(
    data = sep22_offence_sf, aes(fill = object_of_search), 
    color = "white", size = 1.5, alpha = 0.7, shape = 21,
    show.legend = FALSE
  ) + 
  theme_minimal()+
  coord_sf(datum = NA) + #remove coordinates
  facet_wrap(~object_of_search) +
  labs(title = "Stop&Search in London Sep 2022") +
  theme(axis.text = element_blank()) +
  NULL



ggplot() +
  geom_sf(
    data = london_wgs84, fill = "grey18", size = 0.125, colour = "#b2b2b2" ) +
  geom_sf(data = sep22_offence_sf, aes(fill = object_of_search), 
          colour = "#fafafa",
          size = 1.5, alpha = 0.7, shape = 21,
          show.legend = FALSE) + 
  coord_sf(datum = NA) +
  facet_grid(officer_defined_ethnicity ~ object_of_search)+
  labs(title = "Stop&Search in London Sep 2022") +
  theme_minimal()+
  theme(axis.text = element_blank()) +
  NULL


# Count how many S&S happened inside each ward
london_wgs84 <- london_wgs84 %>%
  mutate(count = lengths(
    st_contains(london_wgs84, 
                sep22_offence_sf))) 


ggplot(data = london_wgs84, aes(fill = count)) +
   geom_sf() +
   scale_fill_gradient(low = "#ffffe0", high = "#6e0000")+
  theme_void()


# Each map can be plotted as a static image or viewed interactively using "plot" and "view" modes, respectively. 
# The mode can be set with the function tmap_mode, and toggling between the modes can be done with the ‘switch’ ttm().


tmap::tmap_mode("plot") # static map
tmap::tm_shape(london_wgs84) +
  tm_polygons("count")


tmap::tmap_mode("view") # interactive map
tmap::tm_shape(london_wgs84) +
  tm_polygons("count")


# use mapview and viridis "inferno" colour scale
london_wgs84  %>%
  mapview::mapview(zcol = "count", 
                   at = seq(0, max(london_wgs84$count, na.rm = TRUE), 5), 
                   legend = TRUE,
                   col.regions = inferno(n = 14),
                   layer.name = "Cases per ward")
