library(sp)
library(leaflet)
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)

#####################
## R maps package
#####################
some.na.countries <- c("Canada", "USA", "Mexico")

world_map <- map_data("world", region=some.na.countries)
ggplot(world_map, aes(x=long, y=lat, group=group))+
  geom_polygon(fill="lightgray", color="white")



######################
## Leaflet Choropleth
######################
states <- geojsonio::geojson_read("C:/Users/AHolm/SEIA/OneDrive - SEIA/codebin/datasources/Shapefiles/States/us_states.geo.json", what="sp")

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

basic_choropleth <- leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addPolygons(fillColor = ~pal(density),
        weight = 2,
        opacity = 1,
        color = "#f0f5f8",
        dashArray = "1",
        fillOpacity = 0.7)

basic_choropleth

#######################################
### MAPBOX
#######################################
df <- data.frame(longitude = runif(10, -97.365268, -97.356546), 
                 latitude = runif(10, 32.706071, 32.712210))
coordinates(df) <- ~longitude+latitude

base_mapbox <- "https://api.mapbox.com/styles/v1/aaronleeh/cjms3qjv5aq8e2stbkcf6h5zr/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYWFyb25sZWVoIiwiYSI6IjF0SjNqUUUifQ.0sVfP4L9LWoycJoinMovtA"
map_attr <- " <a href='https://seia.org'>SEIA</a>"

leaflet(df) %>%
  addMarkers() %>%
  addTiles(urlTemplate=base_mapbox, attribution=map_attr)

