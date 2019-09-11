library(scales)
library(janitor)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(forcats)

db_user="PG"
db_pass="PGAH17"
db_host="data.seia.org"
db_port=5432
db_name="seia"

# Get Data
pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user=db_user, password=db_pass, host=db_host, port=db_port, dbname=db_name)
eia_plus = dbGetQuery(con, "select * from products.eia_plus")

################
## HISTORICAL ##
################

smi_capacity = eia_plus %>% 
               filter(sheet == 'Capacity - Current Installs') %>%
               group_by(state_abbr, state_full, sector, year) %>%
               summarise(value=sum(value))

smi_noncsp_annual <- read_csv("C:/tmp/smi_noncsp.csv")

noncsp_states <- unique(smi_noncsp_annual$state_full)
for(state in noncsp_states){
  state_annual <- smi_noncsp_annual %>% filter(state_full==state)
  state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility"))))) + 
                 geom_bar(stat='identity', width=0.5) + 
                 labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + 
                 seia_style() + 
                 scale_fill_manual(values=c("#37b3e5", "#ffe14f", "#2f70af")) + 
                 scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
                 guides(fill=guide_legend(reverse=TRUE))
  ggsave(paste("C:/tmp/factsheets/charts/web/",state,"_web.png", sep=""), width=18.72, height=7.2, dpi=100)
}

smi_csp_annual <- read_csv("C:/tmp/smi_csp.csv")
smi_csp_annual
csp_states <- unique(smi_csp_annual$state_full)

for(state in csp_states){
  state_annual <- smi_csp_annual %>% filter(state_full==state)
  state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility PV", "Utility CSP"))))) + geom_bar(stat='identity', width=0.5) + labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + seia_style() + scale_fill_manual(values=c("#88b551", "#37b3e5", "#ffe14f", "#2f70af")) + scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + guides(fill=guide_legend(reverse=TRUE))
  ggsave(paste("C:/tmp/factsheets/charts/web/",state,"_web.png", sep=""), width=18.72, height=7.2, dpi=100)
}


########################
## FORECAST ############
########################
smi_fcast <- read_csv("C:/tmp/smi_fcast.csv")
fcast_states <- unique(smi_fcast$state_full)

for(state in fcast_states){
  state_annual <- smi_fcast %>% filter(state_full==state)
  sectors <- unique(state_annual)
  print(sectors)
  if (state %in% csp_states){
    state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility PV", "Utility CSP"))))) + 
                   geom_bar(stat='identity', width=0.5) + 
                   labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + 
                   seia_style() + 
                   scale_fill_manual(values=c("#88b551", "#37b3e5", "#ffe14f", "#2f70af")) + 
                   scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
                   guides(fill=guide_legend(reverse=TRUE))
  }
  else{
    state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility"))))) + 
                   geom_bar(stat='identity', width=0.5) + 
                   labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + 
                   seia_style() + 
                   scale_fill_manual(values=c("#37b3e5", "#ffe14f", "#2f70af")) + scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
                   guides(fill=guide_legend(reverse=TRUE))
  }
  ggsave(paste("C:/tmp/factsheets/charts/forecast/",state,"_fcast.png", sep=""), width=18.72, height=7.2, dpi=100)
}
