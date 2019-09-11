library(scales)
library(janitor)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(forcats)
library(rjson)
library(readxl)
library(RPostgreSQL)

# Setup configuration variables
fileloc = paste("C:/tmp/ITC/charts/", state, '.png', sep="")
db_user="PG"
db_pass="PGAH17"
db_host="data.seia.org"
db_port=5432
db_name="seia"

# Get Data
pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user=db_user, password=db_pass, host=db_host, port=db_port, dbname=db_name)
group_data = dbGetQuery(con, "select * from products.itc_2019")
group_data
# Transform data
us_data = group_data %>% group_by(year, scenario) %>%
                        summarise(jobs=sum(jobs, na.rm=T))
                        #summarise(jobs=sum(incremental_mwdc, na.rm=T))
print(us_data %>% filter(year==2019))
us_data$state_full = 'USA'
state_data = group_data %>% group_by(year, state_full, scenario) %>%
                            summarise(jobs=sum(jobs, na.rm=T))
                            #summarise(jobs=sum(incremental_mwdc, na.rm=T))

data = rbind(state_data, us_data)
data
# Make the Chart
#data
#us = data %>% filter(state_full=='USA')




states = unique(data$state_full)
#states = c("USA", "California", "Georgia")
states = c("USA")
for (state in states){
  state_data <- data %>% filter(state_full==state) %>% 
    group_by(year, scenario) %>%
    summarise(jobs=sum(jobs))
  the_scale = get_scale(state_data$jobs)

  scale_min = the_scale[1]
  scale_max = the_scale[2]
  scale_level = the_scale[3]
  print(paste(scale_min, scale_max, scale_level))
  dual_bar <- ggplot(data=state_data %>% filter(year != 2019)) +
    geom_bar(stat="identity", position=position_dodge(), aes(x=factor(year), y=jobs, fill=scenario, colour=scenario)) + 
    scale_fill_manual(values=c("#1f1446", "#37b3e5"))+
    #scale_fill_manual(values=c("#f0f5f8", "#37b3e5"))+
    scale_color_manual(values=c("#1f1446", "#37b3e5"))+
    seia_style()+
    labs(x="", y="Annual Solar Jobs")+
    scale_y_continuous(breaks=seq(250000, 550000, by=50000), expand=expand_scale(mult=c(0,0.02)), label=comma)+
    coord_cartesian(ylim=c(200000,560000))
    #scale_y_continuous(breaks=seq(250000, 500000, by=50000), limits=c(200000, 575000), label=comma)
    #scale_y_continuous(breaks=seq(scale_min, scale_max, by=scale_level), expand=expand_scale(mult=c(0,0.02)), label=comma)  
  #guides(fill=guide_legend(reverse=TRUE))
  ggsave(paste("C:/tmp/ITC/charts/jobs/", state, '_ylim.png', sep=""), width=18, height=7, dpi=300)
  #ggsave(paste("C:/tmp/ITC/charts/deployment/", state, '.png', sep=""), width=18, height=7, dpi=300)
  
}
dual_bar

