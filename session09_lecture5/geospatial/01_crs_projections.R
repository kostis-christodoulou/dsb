library(sf)
library(usethis)
library(tidyverse)
library(rnaturalearth)
library(patchwork)


# we will use the rnatural earth package to get a medium resolution 
# vector map of world countries excluding Antarctica
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica") 

st_geometry(world) # what is the geometry?
# CRS:            +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0

glimpse(world)
# all the magic happens in the last column, called 'geometry:'


# draw a map of the world. you really need just these two lines
ggplot(data = world) +
  geom_sf() 

# we can use different colours/fills, because why not
ggplot(data = world) +
  geom_sf(fill="darkblue", colour="tomato") 

ggplot(data = world) +
  geom_sf(
    mapping = aes(
      geometry = geometry, # use Natural Earth World boundaries
      fill = region_un     # fill colour= country’s region
    ),
    colour = "white",      # white borders between regions
    show.legend = FALSE    # no legend
  )

ggplot(data = world) +
  geom_sf(
    mapping = aes(
      geometry = geometry, #use Natural Earth World boundaries
      fill = pop_est  #fill colour = population estimate
    ),
    colour = "#fafafa",      # white borders between regions
  )


# what happens if we change the crs/projection 

base_map <- ggplot() + 
  geom_sf(data = world, 
          aes(
            geometry = geometry, #use Natural Earth World boundaries
            fill = region_un  #fill colour = country’s region
          ),
          colour = "grey90",      # borders between regions
          show.legend = FALSE    # no legend) +
  )+
  theme_minimal()+
  NULL


base_map

# Longitude/latitude
map_lat_lon <- base_map +
  coord_sf(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_def") +
  labs(title = "Longitude-latitude",
       subtitle = 'crs = "+proj=longlat +ellps=WGS84"')

# Robinson
map_robinson <- base_map +
  coord_sf(crs = "+proj=robin") +
  labs(title = "Robinson",
       subtitle = 'crs = "+proj=robin"')

# Mercator (ew)
map_mercator <- base_map +
  coord_sf(crs = "+proj=merc") +
  labs(title = "Mercator",
       subtitle = 'crs = "+proj=merc"')

# Azimuthal Equidistant
map_azimuthal_equidistant <- base_map +
  coord_sf(crs = "+proj=aeqd") +  # Gall Peters / Equidistant cylindrical
  labs(title = "Azimuthal Equidistant",
       subtitle = "crs = +proj=aeqd")

#use patchwork to arrange 4 maps in one page
(map_lat_lon / map_mercator) | ( map_robinson / map_azimuthal_equidistant)


