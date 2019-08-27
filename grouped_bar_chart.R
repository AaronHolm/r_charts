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

#group_data = read_excel('C:/tmp/ITC/Clean Files/WoodMac_IO_run_6_calibrated_v2_chart_data.xlsx')
# Get Data
pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user=db_user, password=db_pass, host=db_host, port=db_port, dbname=db_name)
group_data = dbGetQuery(con, "select * from products.itc_2019")

# Transform data
us_data = group_data %>% group_by(year, scenario) %>%
                        summarise(jobs=sum(jobs, na.rm=T))
us_data$state_full = 'USA'
us_data
state_data = group_data %>% group_by(year, state_full, scenario) %>%
                            summarise(jobs=sum(jobs, na.rm=T))

data = rbind(state_data, us_data)

# Make the Chart
#data
#us = data %>% filter(state_full=='USA')

states = unique(group_data$state_full)
for (state in states){
  state_data <- group_data %>% filter(state_full==state) %>% 
    group_by(year, scenario) %>%
    summarise(jobs=sum(jobs))
  
  dual_bar <- ggplot(data=state_data, aes(x=factor(year), y=jobs, fill=scenario)) +
    geom_bar(stat="identity", position=position_dodge()) + 
    scale_fill_manual(values=c("#37b3e5", "#1f1446"))+
    seia_style()+
    scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) 
  #guides(fill=guide_legend(reverse=TRUE))
  
  ggsave(fileloc, width=18, height=7, dpi=300)
  
}

