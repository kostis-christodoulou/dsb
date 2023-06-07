library(mapboxapi)
library(mapdeck)
library(tidyverse)
library(leaflet)
library(tidygeocoder)

# copy/paste your mapbox access token below
mb_access_token("pk.ey......", install = TRUE)

# mb_isochrone
walk_5min <- mb_isochrone("Sammy Ofer Centre",
                          profile = "walking",
                          time = 7)

leaflet(walk_5min) %>%
  addMapboxTiles(style_id = "streets-v11",
                 username = "mapbox") %>%
  addPolygons()


# mb_isochrone + mapdeck interactive map
isochrones <- mb_isochrone("NW1 4SA", 
                           time = c(5, 10, 20),
                           profile = "cycling") 

mapdeck(style = mapdeck_style("light")) %>%
  add_polygon(data = isochrones, 
              fill_colour = "time",
              fill_opacity = 0.5,
              legend = TRUE)


# routing
my_route <- mb_directions(
  origin = "NW1 4SA",
  destination = "British Library",
  profile = "walking",
  steps = TRUE,
 language = "en" #https://docs.mapbox.com/api/navigation/directions/#instructions-languages # en is the default option
#  language = "el-GR" #https://docs.mapbox.com/api/navigation/directions/#instructions-languages
# language = "zh-CN" #https://docs.mapbox.com/api/navigation/directions/#instructions-languages
# language = "es" #https://docs.mapbox.com/api/navigation/directions/#instructions-languages
)

glimpse(my_route)
my_route$instruction 



leaflet(my_route) %>%
  addMapboxTiles(style_id = "light-v9",
                 username = "mapbox") %>%
  addPolylines()


my_route |> 
  summarise(total_distance = sum(distance),
            total_time = sum(duration))

# Geocoding
# with mapboxapi
lbs1 <- mb_geocode("London Business School") 

# with tidygeocoder
lbs2 <- geo("London Business School",
            method = "osm",
            full_results = TRUE)




