library(scales)
library(janitor)
library(tidyverse)
library(ggplot2)
#library(gganimate)
#library(gifski)
library(forcats)
library(DBI)
library(RPostgreSQL)


# Get Data
pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user=db_user, password=db_pass, host=db_host, port=db_port, dbname=db_name)
eia_plus = dbGetQuery(con, "select * from products.eia_plus")

csp_states = c("California", "Nevada", "Florida", "Arizona")
weak_states <- c("Alaska", "North Dakota", "West Virginia")

################
## HISTORICAL ##
################

smi_capacity = eia_plus %>% 
               filter(sheet == 'Capacity - Current Installs', time_period != '2010PreCumulative') %>%
               group_by(state_abbr, state_full, sector, type, year) %>%
               summarise(value=sum(value))

smi_capacity
#smi_noncsp_annual <- read_csv("C:/tmp/smi_noncsp.csv")
smi_noncsp_annual <- smi_capacity %>% filter(!state_full %in% csp_states)
smi_noncsp_annual

noncsp_states <- unique(smi_noncsp_annual$state_full)
for(state in noncsp_states){
  state_annual <- smi_noncsp_annual %>% filter(state_full==state)
  if(state %in% weak_states){
    state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential"))))) + 
      geom_bar(stat='identity', width=0.5) + 
      labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + 
      seia_style() + 
      scale_fill_manual(values=c("#ffe14f", "#2f70af")) + 
      scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
      scale_x_discrete(labels=c("2019"="2019 Q2")) +
      guides(fill=guide_legend(reverse=TRUE))
    
  } else if(state == 'Puerto Rico'){
    state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Utility"))))) + 
      geom_bar(stat='identity', width=0.5) + 
      labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + 
      seia_style() + 
      scale_fill_manual(values=c("#37b3e5")) + 
      scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
      scale_x_discrete(labels=c("2019"="2019 Q2")) +
      guides(fill=guide_legend(reverse=TRUE))
    
  }
  else{
    state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility"))))) + 
                 geom_bar(stat='identity', width=0.5) + 
                 labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + 
                 seia_style() + 
                 scale_fill_manual(values=c("#37b3e5", "#ffe14f", "#2f70af")) + 
                 scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
                 scale_x_discrete(labels=c("2019"="2019 Q2")) + 
                 guides(fill=guide_legend(reverse=TRUE))
  }
  ggsave(paste("C:/tmp/factsheets/charts/web/",state,".png", sep=""), width=18.72, height=7.2, dpi=100)
}

#smi_csp_annual <- read_csv("C:/tmp/smi_csp.csv")
smi_csp_annual <- smi_capacity %>% filter(state_full %in% csp_states)

smi_csp_annual$sector = apply(smi_csp_annual, 1, function(x) 
  if(x['sector'] == 'Residential'){'Residential'}
  else if(x['sector'] == 'Non-Residential'){'Non-Residential'}
  else if(x['sector'] == 'Utility' & x['type'] == 'CSP'){'Utility CSP'}
  else {'Utility PV'})


smi_csp_annual
#csp_states <- unique(smi_csp_annual$state_full)

for(state in csp_states){
  state_annual <- smi_csp_annual %>% filter(state_full==state)
  state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility PV", "Utility CSP"))))) + 
                 geom_bar(stat='identity', width=0.5) + 
                 labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + 
                 seia_style() + 
                 scale_fill_manual(values=c("#88b551", "#37b3e5", "#ffe14f", "#2f70af")) + 
                 scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
                 scale_x_discrete(labels=c("2019"="2019 Q2")) + 
                 guides(fill=guide_legend(reverse=TRUE))
  ggsave(paste("C:/tmp/factsheets/charts/web/",state,".png", sep=""), width=18.72, height=7.2, dpi=100)
}


########################
## FORECAST ############
########################

#smi_fcast <- read_csv("C:/tmp/smi_fcast.csv")
smi_fcast <- eia_plus %>% filter(sheet == 'Capacity - PV Forecast', year %in% c("2019E", "2020E", "2021E", "2022E", "2023E")) %>%
                      group_by(state_abbr, state_full, sector, type, year) %>%
                      summarise(value=sum(value))

smi_fcast$sector = apply(smi_fcast, 1, function(x) 
  if(x['state_full'] %in% csp_states){
    if(x['sector'] == 'Residential'){'Residential'}
    else if(x['sector'] == 'Non-Residential'){'Non-Residential'}
    else if(x['sector'] == 'Utility' & x['type'] == 'CSP'){'Utility CSP'}
    else {'Utility PV'}
  }
  else{x['sector']})


smi_fcast <- rbind(smi_csp_annual %>% filter(year != '2019'), smi_noncsp_annual %>% filter(year != '2019'), smi_fcast)

smi_fcast
fcast_states <- unique(smi_fcast$state_full)
fcast_states
curr_q = "2019 Q2"
for(state in fcast_states){
  state_annual <- smi_fcast %>% filter(state_full==state)
  #print(head(state_annual))
  sectors <- unique(state_annual$sector)
  #print(sectors)
  print(paste(state, length(sectors)))
  #print(sectors)
  
  if (state %in% csp_states){
    state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility PV", "Utility CSP"))))) + 
                   geom_bar(stat='identity', width=0.5) + 
                   labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + 
                   seia_style() + 
                   scale_fill_manual(values=c("#88b551", "#37b3e5", "#ffe14f", "#2f70af")) + 
                   scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
                   scale_x_discrete(labels=c("2019"="2019 Q2")) +
                   guides(fill=guide_legend(reverse=TRUE))
  } else if(state %in% weak_states){
    state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential"))))) + 
      geom_bar(stat='identity', width=0.5) + 
      labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + 
      seia_style() + 
      scale_fill_manual(values=c("#ffe14f", "#2f70af")) + 
      scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
      scale_x_discrete(labels=c("2019"="2019 Q2")) +
      guides(fill=guide_legend(reverse=TRUE))
    
  } else if(state == 'Puerto Rico'){
    state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Utility"))))) + 
      geom_bar(stat='identity', width=0.5) + 
      labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + 
      seia_style() + 
      scale_fill_manual(values=c("#37b3e5")) + 
      scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
      scale_x_discrete(labels=c("2019"="2019 Q2")) +
      guides(fill=guide_legend(reverse=TRUE))
    
  }
  else{
    state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility"))))) + 
                   geom_bar(stat='identity', width=0.5) + 
                   labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + 
                   seia_style() + 
                   scale_fill_manual(values=c("#37b3e5", "#ffe14f", "#2f70af")) + 
                   scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
                   scale_x_discrete(labels=c("2019"="2019 Q2")) +
                   guides(fill=guide_legend(reverse=TRUE))
  }
  ggsave(paste("C:/tmp/factsheets/charts/forecast/",state,".png", sep=""), width=18.72, height=7.2, dpi=100)
  
}

