library(osrm)
library(osmdata)
library(stplanr)
library(mapview)

#> Data: (c) OpenStreetMap contributors, ODbL 1.0 - http://www.openstreetmap.org/copyright
#> Routing: OSRM - http://project-osrm.org/

trip <- route(
  from = "London Business School",
  to = "Rome",
  route_fun = osrmRoute
)

trip

#> Most common output is sf
mapview::mapview(trip)
