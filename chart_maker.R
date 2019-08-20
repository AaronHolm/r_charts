library(scales)
library(janitor)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(forcats)

library(RPostgreSQL)

# Get Data
pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user="PG", password="PGAH17", host="data.seia.org", port=5432, dbname="seia")
capacity = dbGetQuery(con, "select * from products.eia_plus where sheet = 'Capacity - Current Installs'")

# Transform Data
states <- unique(capacity$state_full)
states
for(state in states){
  state_capacity <- capacity %>% filter(state_full==state)
  state_chart <- ggplot(state_capacity, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility"))))) +
    geom_bar(stat='identity', width=0.5) + 
    labs(y="Capacity (MW)", x="", title=paste(state, "Annual Solar Installations"), caption="Source: pRty") + 
    seia_style() +
    scale_fill_manual(values=c("#37b3e5", "#ffe14f", "#2f70af")) +
    guides(fill=guide_legend(reverse=TRUE)) +
    scale_y_continuous(expand=expand_scale(mult=c(0,0.02))) +
    scale_x_discrete(labels=c("2019"="2019 Q1"))
  ggsave(paste("C:/tmp/R/", state, ".png", sep=""), width=18, height=7, dpi=100)
}
state_chart
# I'm adding a line to this script
# I'm adding a second line to this script