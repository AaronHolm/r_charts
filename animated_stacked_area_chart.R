library(ggplot2)
library(tidyverse)
library(gganimate)
library(transformr)
library(RPostgreSQL)
# Get Data
pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user=db_user, password=db_pass, host=db_host, port=db_port, dbname=db_name)
group_data = dbGetQuery(con, "select * from products.itc_2019")

# Transform the Data
basecase <- group_data %>% 
  filter(scenario=="Basecase") %>%
  #filter(run_type=="O & M", scenario=="Basecase") %>%
  #filter(run_type=="Installers", scenario=="Basecase") %>%
  group_by(state_full, year) %>%
  #summarise(MW=sum(jobs))
  #summarise(MW=sum(cumulative_mwdc))
  summarise(MW=sum(incremental_mwdc))

basecase <- basecase[c("state_full", "year", "MW")]

us_basecase <- basecase %>% group_by(year) %>% summarise(MW=sum(MW))
us_basecase$state_full <- 'USA'
basecase <- bind_rows(basecase, us_basecase)

extension <- group_data %>%
  filter(scenario=="Extension") %>%
  #filter(run_type=="O & M", scenario=="Extension") %>%
  #filter(run_type=="Installers", scenario=="Extension") %>%
  group_by(state_full, year) %>%
  #summarise(MW_ext=sum(cumulative_mwdc))
  summarise(MW_ext=sum(incremental_mwdc))
#summarise(MW_ext=sum(jobs))

extension <- extension[c("state_full", "year", "MW_ext")]

us_extension <- extension %>% group_by(year) %>% summarise(MW_ext=sum(MW_ext))
us_extension$state_full <- 'USA'
extension <- bind_rows(extension, us_extension)

extension <- merge(basecase, extension, by=c("state_full", "year"))
extension$MW <- with(extension, ifelse(extension$MW_ext - extension$MW < 0, 0, extension$MW_ext - extension$MW))
#extension$MW <- extension$MW_ext - extension$MW
#df$D <- with(df, ifelse(C %in% "+", A - B, B - A))
extension <- extension[c("state_full", "year", "MW")]

extension$scenario <- "Extension"
basecase$scenario <- "Basecase"

cumulative_mw <- bind_rows(basecase, extension)

# Make the Chart
#states <- unique(cumulative_mw$state_full)
states <- c("USA")
#for(state in states){
state = "USA"  
  #state_data <- cumulative_mw %>% filter(state_full==state) %>% filter(year != 2019)
  base <- basecase %>% filter(state_full==state) %>% filter(year != 2019)
  ext <- cumulative_mw %>% filter(state_full==state) %>% filter(year != 2019)
  ext  
  the_scale = get_scale(ext$MW)
  scale_min = the_scale[1]
  scale_max = the_scale[2]
  scale_level = the_scale[3]
  print(paste(scale_min, scale_max, scale_level))
  
  deployment_chart <- ggplot(ext, aes(x=factor(year), y=MW, fill=scenario, group=scenario))+
    geom_line(data=base, position = position_stack(reverse = TRUE), size=1.1, mapping=aes(x=factor(year), y=MW, colour=scenario))+
    geom_area(position = position_stack(reverse = TRUE))+
    #scale_fill_manual(values=c(alpha(c("#37b3e5", "#1f1446"), 0.7)))+
    scale_fill_manual(values=c(alpha("#1f1446", 0), alpha("#37b3e5", 0.7)))+
    scale_color_manual(values=c("#1f1446", "#37b3e5"))+
    scale_y_continuous(breaks=seq(10000,40000, by = 10000), expand=expand_scale(mult=c(0,0.02)), label=comma)+
    #scale_y_continuous(breaks=seq(scale_min,scale_max, by = scale_level), expand=expand_scale(mult=c(0,0.02)), label=comma) +
    #scale_y_continuous(breaks = seq(1, max(cumulative_mw$MW, na.rm=TRUE), by=(max(cumulative_mw$MW, na.rm=TRUE)/floor(log(abs(cumulative_mw$MW))))),expand=expand_scale(mult=c(0,0.02)), label=comma) +
    #scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) +
    scale_x_discrete(expand=expand_scale(mult=c(0.02,0.02)))+
    #labs(y="Cumulative Solar Installations (MW)", x="") +
    #labs(y="Annual Solar Jobs", x="") +
    labs(y="Annual Solar Installations (MW)", x="") +
    seia_style()+
    #theme(axis.title.y = ggplot2::element_text(family="Roboto Black", size=6, color="#1f1446", face="bold", margin=margin(0,10, 0, 0)), 
    #axis.text = ggplot2::element_text(family="Roboto Black", size=4, color="#1f1446", face="bold"))+
    guides(fill=guide_legend(reverse=TRUE))+
    transition_reveal(ext$year)
  anim_save("C:/tmp/ITC/charts/deployment/USA_res150.gif", animate(deployment_chart, fps=60, height = 700, width =1800, end_pause=50, res=125))
  #gg_animate(deployment_chart)
  #ggsave(paste("C:/tmp/ITC/charts/deployment/", state, "_cumulative_deployment.png"), width=18, height=7, dpi=300)
  
  
  #ggsave(paste("C:/tmp/ITC/charts/deployment/", state, "_incremental_deployment.png"), width=18, height=7, dpi=300)
  #ggsave(paste("C:/tmp/ITC/charts/jobs/", state, ".png"), width=18, height=7, dpi=300)
  deployment_chart
#}
deployment_chart

