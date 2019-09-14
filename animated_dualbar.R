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
con = dbConnect(pg, user=db_user, password=db_pass, host=db_host, port=db_port, dbname=db_name)
group_data = dbGetQuery(con, "select * from products.itc_2019")
group_data
# Transform data
us_data = group_data %>% group_by(year, scenario) %>%
  summarise(jobs=sum(jobs, na.rm=T))
#summarise(jobs=sum(incremental_mwdc, na.rm=T))
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
  state = "USA"
  state_data <- data %>% filter(state_full==state) %>% 
    group_by(year, scenario) %>%
    summarise(jobs=sum(jobs))

  state_base <- state_data %>% filter(scenario=="Basecase")
  state_ext <- state_data %>% filter(scenario=="Extension")
  state_base$base_jobs <- state_base$jobs
  state_base
  state_ext$ext_jobs <- state_ext$jobs
  keeps <- c("year", "base_jobs")
  state_base <- state_base[keeps]
  state_base
  keeps <- c("year", "ext_jobs")
  state_ext <- state_ext[keeps]
  state_ext
  state_data3 <- merge(state_base, state_ext, by="year")
  state_data3
  state_data3$jobs <- (state_data3$ext_jobs - state_data3$base_jobs)
  state_base$scenario <- "Basecase"
  state_base$jobs <- state_base$base_jobs
  state_data3$scenario <- "Extension"
  keeps <- c("year", "scenario", "jobs")
  state_data3 <- state_data3[keeps]
  state_base <- state_base[keeps]
  state_data3
  state_base
  state_df <- bind_rows(state_base, state_data3)
  state_df
  #state_data2 <- state_data
  #state_data2$jobs[state_data2$scenario=='Extension'] <- 0
  #state_data2$year2 <- state_data$year
  #keeps <- c("year2","scenario", "jobs")
  #state_data2 <- state_data2[keeps]
  #state_data
  state_data2
  #the_scale = get_scale(state_data$jobs)
  #the_scale = get_scale(state_df$jobs)
  
  #scale_min = the_scale[1]
  #scale_max = the_scale[2]
  #scale_level = the_scale[3]
  #print(paste(scale_min, scale_max, scale_level))
  state_df <- state_df %>% filter(year != 2019)
  state_df
  dual_bar <- ggplot(state_df) +
    #geom_bar(data=state_data2, aes(x=factor(year2), y=jobs, fill=scenario, colour=scenario), stat="identity", position=position_dodge()) +
    #geom_bar(stat="identity", position=position_dodge(), aes(fill=scenario, colour=scenario)) + 
    #geom_col(data=state_data2, position=position_dodge(), aes(x=factor(year2), y=jobs, fill=scenario, colour=scenario)) + 
    geom_bar(stat="identity", position="stack", aes(x=factor(year), y=jobs, fill=factor(scenario))) + 
    #scale_fill_manual(values=c("#37b3e5", "#1f1446"))+
    #scale_color_manual(values=c("#1f1446", "#37b3e5"))+
    seia_style()+
    labs(x="", y="Annual Solar Jobs")+
    scale_y_continuous(breaks=seq(250000, 550000, by=50000), expand=expand_scale(mult=c(0,0.02))) + #, label=comma
    coord_cartesian(ylim=c(200000,560000)) #+
    #transition_reveal(year)
  #scale_y_continuous(breaks=seq(250000, 500000, by=50000), limits=c(200000, 575000), label=comma)
  #scale_y_continuous(breaks=seq(scale_min, scale_max, by=scale_level), expand=expand_scale(mult=c(0,0.02)), label=comma)  
  #guides(fill=guide_legend(reverse=TRUE))

  #anim_save("C:/tmp/ITC/charts/USA_jobs_stacked.gif", animate(dual_bar, fps=30, height = 700, width=1800))
  dual_bar
  ggsave("C:/tmp/ITC/charts/USA_jobs_stacked.png", width=18, height=7, dpi=300)
  #ggsave(paste("C:/tmp/ITC/charts/jobs/", state, '_ylim.png', sep=""), width=18, height=7, dpi=300)
  #ggsave(paste("C:/tmp/ITC/charts/deployment/", state, '.png', sep=""), width=18, height=7, dpi=300)
  
}
dual_bar

