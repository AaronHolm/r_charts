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

group_data = read_excel('C:/tmp/ITC/Clean Files/WoodMac_IO_run_6_calibrated_v2_chart_data.xlsx')
us_data = group_data %>% group_by(year, scenario) %>%
                        summarise(jobs=sum(jobs, na.rm=T))
us_data$state_full = 'USA'
us_data
state_data = group_data %>% group_by(year, state_full, scenario) %>%
                            summarise(jobs=sum(jobs, na.rm=T))

data = rbind(state_data, us_data)
data
us = data %>% filter(state_full=='USA')
us
california <- group_data %>% filter(state_full=='California') %>% 
                             group_by(year, scenario) %>%
                             summarise(jobs=sum(jobs))
california


dual_bar <- ggplot(data=us, aes(x=factor(year), y=jobs, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values=c("#37b3e5", "#1f1446"))+
  seia_style()+
  scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) 
  #guides(fill=guide_legend(reverse=TRUE))
dual_bar
#ggsave("C:/tmp/ITC/dual_bar_chart.png", width=18, height=7, dpi=300)
