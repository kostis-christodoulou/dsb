library(googlesheets4)
library(sf)
library(opencage) # for geocoding addresses
library(usethis)
library(hrbrthemes) # hrbrmstr/hrbrthemes
library(tidyverse)
library(kableExtra)
library(rnaturalearth)
library(tmap)
library(tidygeocoder)

googlesheets4::gs4_auth() # google sheets authorisation

#load countries_visited googlesheets
countries_e628 <- read_sheet("https://docs.google.com/spreadsheets/d/1M597P_NWZ88s_kLNL_pxaN2DVKa2Y_aQLfKvwvJYqKo/edit?usp=sharing
") 

countries_e628 <- countries_e628 %>% 
  count(country, sort=TRUE)

geocoded <- countries_e628 %>% 
  
  mutate(
    address_geo = purrr::map(country, geo, method = "osm")
  ) %>% 
  unnest_wider(address_geo) 



geocoded %>% 
  kable()%>%  # print a table with geocoded addresses
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# we will use the rnatural earth package to get a medium resolution 
# vector map of world countries excl. Antarctica
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica") 

st_geometry(world) # what is the geometry?
# CRS:            +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0

ggplot(data = world) +
  geom_sf() + # the first two lines just plot the world shapefile
  geom_point(data = geocoded, # then we add points
             aes(x = long, y = lat), 
             size = 2, 
             colour = "#001e62") +
  theme_void()


world_visited <- left_join(world, geocoded,  by=c("admin" = "country" )) %>% 
  mutate(visited = if_else (!is.na(lat), "visited", "not visited")
  )

ggplot(world_visited)+
  geom_sf(aes(fill=visited),     
          show.legend = FALSE)+    # no legend
  scale_fill_manual(values=c('#f0f0f0', '#3182bd'))+
  coord_sf(datum = NA) +
  theme_void()+
  labs(title="Where do E628 participants come from?")+
  NULL
