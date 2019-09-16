install.packages("devtools")
devtools::install_github("UrbanInstitute/urbnmapr")
library(tidyverse)
library(urbnmapr)

map_test <- ggplot() + 
  geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
               fill = "grey", color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  seia_style()

map_test

library(sp)
library(leaflet)
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
library(gganimate)
library(RPostgreSQL)

pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user=db_user, password=db_pass, host=db_host, port=db_port, dbname=db_name)
itc_data = dbGetQuery(con, "select * from products.itc_2019")

######################
## Leaflet Choropleth
######################
states <- geojsonio::geojson_read("C:/Users/AHolm/SEIA/OneDrive - SEIA/codebin/datasources/Shapefiles/States/us_states.geo.json", what="sp")
print(states)

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

map_test <- ggplot() + 
  geom_polygon(data = states, mapping = aes(x = long, y = lat, group = group),
               fill = "grey", color = "white") +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  theme_void()+
  coord_map()
map_test


library(sf)
library(tidyverse)

states_sf <- read_sf("C:/Users/AHolm/SEIA/OneDrive - SEIA/codebin/datasources/Shapefiles/States/us_states.geo.json")

sf_map <- ggplot() + 
          geom_sf(data=states) +
          theme_void()+
          coord_sf()
sf_map

usa <- map_data("usa")

usa_map <- ggplot(usa, aes(x = long, y = lat, group = group)) +
           geom_polygon() +
           coord_map() +
           theme_nothing()
usa_map

state <- map_data("state")
group_data = dbGetQuery(con, "select * from products.itc_2019")
group_data$region <- tolower(group_data$state_full)
group_data <- group_data %>% 
                select(region, earnings, year) %>%
                group_by(region, year) %>%
                summarise(earnings = sum(earnings))

group_data
#group_data <- group_data %>% filter(year==2020)

#state_map_data <- merge(state, group_data, by="region")
state_map_data <- merge(state, group_data)
state_map_data
#state_map_data <- state_map_data %>% filter(year==2020)
#state_map <- ggplot(state_map_data, aes(x = long, y = lat, fill = cumulative_mwdc, group = group)) +
#             geom_polygon(col = "white") +
#             coord_map() +
#             theme_nothing()
#state_map
#ggsave("C:/tmp/r/state_map_test.png", width=18, height=7, dpi=1000)
group_data
us <- map_data("state")

state_map_map <- ggplot(state_map_data) +
                 #geom_map(data=us, map=us, aes(x=long, y=lat, map_id=region), fill="#ffffff", color="#ffffff", size=0.15) +
                 #geom_map(data=group_data, map=us, aes(fill=cumulative_mwdc, map_id=region), color="#ffffff", size=0.15) +
                 geom_map(map=us, aes(x=long, y=lat, map_id=region,fill=earnings), color="#ffffff", size=0.15) +
                 scale_fill_continuous(low='#f0f5f8', high='#1f1446', guide='colorbar')+
                 #scale_fill_gradient(low='#f0f5f8', high='#1f1446', guide='colorbar')+
                 labs(x=NULL, y=NULL)+
                 coord_map("albers", lat0 = 39, lat1 = 45)+
                 theme(panel.border = element_blank())+
                 theme(panel.background = element_blank())+
                 theme(axis.ticks = element_blank())+
                 theme(axis.text = element_blank())+
                 transition_reveal(year)
anim_map <- animate(state_map_map,fps=30, height = 700, width =1800)
#anim_map
#state_map_map
anim_save("C:/tmp/R/state_mwdc.gif", anim_map)
#anim_save("C:/tmp/r/state_mwdc.gif", animate(state_map_map, fps=30, height = 700, width =1800))

theme_nothing <- function(base_size = 12, legend = FALSE){
  if(legend){
    theme(
      axis.text =          element_blank(),
      axis.title =         element_blank(),
      panel.background =   element_blank(),
      panel.grid.major =   element_blank(),
      panel.grid.minor =   element_blank(),
      axis.ticks.length =  unit(0, "cm"),
      panel.spacing =      unit(0, "lines"),
      plot.margin =        unit(c(0, 0, 0, 0), "lines")
    )
  } else {
    theme(
      line =               element_blank(),
      rect =               element_blank(),
      text =               element_blank(),
      axis.ticks.length =  unit(0, "cm"),
      legend.position =    "none",
      panel.spacing =      unit(0, "lines"),
      plot.margin =        unit(c(0, 0, 0, 0), "lines")
    )
  }
}
