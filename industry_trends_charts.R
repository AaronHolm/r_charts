library(dplyr)
library(scales)
library(janitor)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(forcats)
library(extrafont)
library(RPostgreSQL)

pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user="PG", password="PGAH17", host="data.seia.org", port=5432, dbname="seia")

font_import(prompt=FALSE)
loadfonts(device = "win")

seia_style <- function() {
       font <- "Roboto"
       
       ggplot2::theme(
               
                 #Text format:
                 #This sets the font, size, type and colour of text for the chart's title
                 plot.title = ggplot2::element_text(family=font,
                                                     size=32,
                                                     face="bold",
                                                     color="#1f1446", hjust=0.5),
               #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
               plot.subtitle = ggplot2::element_text(family=font,
                                                       size=22,
                                                     margin=ggplot2::margin(9,0,9,0)),
               plot.caption = element_text(hjust=1, family=font, size=16, face='bold'),
               plot.margin = unit(c(.5,2.5,.5, .5), "cm"),
               #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
               
               #Legend format
               #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
               legend.position = "bottom",
               legend.text.align = 5,
               legend.background = ggplot2::element_blank(),
               legend.title = ggplot2::element_blank(),
               legend.key = ggplot2::element_blank(), 
               legend.key.size=unit(1.5, "lines"), 
               legend.text = ggplot2::element_text(family=font,
                                                     size=16,
                                                    color="#1f1446", 
                                                    face="bold",
                                                   margin=margin(r=10, l=5, unit="pt"), hjust=0),
               legend.spacing.x = unit(.1, "cm"),
     
                 #Axis format
                 #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
                 axis.title.y = ggplot2::element_text(family=font, size=20, color="#1f1446", face="bold", margin=margin(0,10, 0, 0)), axis.title.x = ggplot2::element_blank(), axis.text = ggplot2::element_text(family=font,
                                                                                                                                                                                                                   size=18,
                                                                                                                                                                                                                  color="#1f1446", 
                                                                                                                                                                                                                  face="bold"),
               axis.text.x = ggplot2::element_text(angle=0, hjust=.5),
               axis.text.y.right = ggplot2::element_text(margin = unit(c(5, 5, 0, 0), "mm")),
               axis.ticks = ggplot2::element_blank(),
               axis.line = ggplot2::element_blank(),
               
                 #Grid lines
                 #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
                panel.grid.minor = ggplot2::element_blank(),
               panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
               panel.grid.major.x = ggplot2::element_blank(),
               #panel.border = element_rect(colour = "#1f1446", fill=NA, size=1),
                 #Blank background
                 #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
                 panel.background = ggplot2::element_rect(fill="#f0f5f8", colour="slategrey", size=0.5, linetype="solid"),
               
                 #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
               strip.background = ggplot2::element_rect(fill="white"),
               strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
           )
}

# Grab EIA+
#eia_plus <- read_csv("C:/tmp/eia_plus.csv", col_types=cols(.default=col_guess(), year=col_character()))
#eia_plus <- read_csv("C:/tmp/eia_plus.csv", col_types=cols(.default=col_character(), value=col_double(), year=col_character()))
eia_plus = dbGetQuery(con, "select * from products.eia_plus")
#########################
# Title Slide
# PPT #1 

#########################
# Solar Growth w/ ITC
# PPT #2
# 2000-2018, includes CSP and annotations
smi_histfcast <- eia_plus %>% filter((time_period != '2010PreCumulative' & !(time_period %in% c("2019", "2020E", "2021E", "2022E", "2023E", "2024E"))) & (sheet=='Capacity - National Historical 2000-forecast') & (sector %in% c('Residential', 'Non-Residential', 'Utility')))
#smi_current <- eia_plus %>% filter(sheet=='Capacity - Current Installs' & time_period != '2010PreCumulative')
smi_csp <- eia_plus %>% filter(sheet=='Capacity - Current Installs' & time_period != '2010PreCumulative' & type=='CSP')


cols_to_keep <- c("sector", "year", "value", "type")
smi_current <- smi_histfcast[cols_to_keep]
smi_current <- smi_current %>% mutate(sector=ifelse(sector=='Utility', 'Utility PV', sector))

smi_us <- smi_current %>% 
  group_by(sector, year) %>%
  summarise(value=sum(value))
smi_us

smi_csp <- smi_csp %>% mutate(sector=ifelse(sector=='Utility', 'Utility CSP', 'Utility'))
smi_csp
smi_csp <- smi_csp %>%
  group_by(sector, year) %>%
  summarise(value=sum(value))

itc_data <- merge(smi_us, smi_csp, by=c("sector", "year", "value"), all=TRUE)
itc_data <- itc_data %>% filter(year != '2010PreCum')
itc_data

itc_chart <- ggplot(itc_data, aes(x=year, y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility PV", "Utility CSP"))))) + 
             geom_bar(stat='identity', width=0.5) + 
             labs(y="Capacity (MWdc)", x="", title="Annual U.S. Solar Installations", caption="Source: SMI") + 
             seia_style() + 
             scale_fill_manual(values=c("#88b551", "#37b3e5", "#ffe14f", "#2f70af")) + 
             scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
             guides(fill=guide_legend(reverse=TRUE)) + 
             annotate("text", x="2003", y=4500, label="ITC Created", family="Roboto Black", fontface="bold", size=6) + 
             geom_segment(aes(x="2003", y=4000, xend="2005", yend=350), colour='#1f1446', size=1.5,arrow = arrow(length = unit(0.25, "cm"), type="closed")) +  
             annotate("text", x="2005", y=7500, label="ITC Extended", family="Roboto Black", fontface="bold", size=6) + 
             geom_segment(aes(x="2005", y=7000, xend="2007", yend=500), colour='#1f1446', size=1.5,arrow = arrow(length = unit(0.25, "cm"), type="closed")) +  
             annotate("text", x="2007", y=10000, label="ITC Extended & Expanded", family="Roboto Black", fontface="bold", size=6) + 
             geom_segment(aes(x="2007", y=9500, xend="2008", yend=750), colour='#1f1446', size=1.5,arrow = arrow(length = unit(0.25, "cm"), type="closed")) +  
             annotate("text", x="2010", y=12500, label="ITC Extended", family="Roboto Black", fontface="bold", size=6) + 
             geom_segment(aes(x="2010", y=12000, xend="2015", yend=8500), colour='#1f1446', size=1.5,arrow = arrow(length = unit(0.25, "cm"), type="closed")) #+
             #transition_reveal(as.numeric(year))+
             #ease_aes("linear")

itc_chart


#animate(itc_chart, nframes=750, fps=25, end_pause=50, width=1200, height=900)
animate(itc_chart, renderer=gifski_renderer(), width=1200, height=600, res=75, end_pause=75)
anim_save("itc_2.gif", animation = last_animation(), path = "C:/tmp/industry trends/")  


ggsave("C:/tmp/industry trends/itc_growth_2019Q2.png", width=18.72, height=7.2, dpi=300)  


#########################
# TSF Jobs by Sector
# PPT #
jobs_data <- eia_plus %>% filter(sheet=='TSF Jobs by Sector', data_source=='TSF 2018')
jobs_data
cols_to_keep <- c("sector", "year", "value")
jobs_data <- jobs_data[cols_to_keep]

jobs_data$sector[which(jobs_data$sector == 'Installation and Project Development')] = 'Installation'
jobs_data$sector[which(jobs_data$sector == 'Wholesale Trade and Distribution')] = 'Sales & Distribution'
jobs_data$sector[which(jobs_data$sector == 'All Others')] = 'Other'
jobs_data$sector[which(jobs_data$sector == 'Operations and Maintenance')] = 'Operations & Maintenance'

jobs_chart <- ggplot(jobs_data, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Installation", "Manufacturing", "Sales & Distribution", "Operations & Maintenance", "Other"))))) + 
  geom_bar(stat='identity', width=0.5) + 
  labs(y="Number of Jobs", x="", title="Solar Industry Jobs by Sector") + 
  seia_style() + 
  scale_fill_manual(values=c("#ad6eae", "#88b551", "#37b3e5", "#ffe14f", "#2f70af")) + 
  scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
  guides(fill=guide_legend(reverse=TRUE))
jobs_chart
ggsave("C:/tmp/industry trends/jobs.png", width=18.72, height=7.2, dpi=300)


###############################
# Deployment from Falling Prices
# PPT #4
fall_cap <- eia_plus %>% filter(sheet=='Capacity - Current Installs' & !(time_period %in% c('2010PreCumulative', '2019E', '2020E', '2021E', '2022E', '2023E', '2024E')) & sector %in% c("Residential", "Non-Residential", "Utility"))
fall_cap <- fall_cap %>% 
  group_by(year) %>%
  summarise(value=sum(value))
cols_to_keep <- c("year", "value")
fall_cap <- fall_cap[cols_to_keep]
colnames(fall_cap)[which(names(fall_cap)=='value')] <- "mw"

E2019 <- eia_plus %>% filter(sheet=='Capacity - PV Forecast' & time_period == '2019E')
E2019 <- E2019 %>%
          group_by(year) %>%
          summarise(value=sum(value))
E2019 <- E2019[cols_to_keep]
E2019$year[E2019$year=='2019E'] <- 2019
A2019 <- fall_cap %>% filter(year=='2019')
TOT2019 <- merge(E2019, A2019, all=TRUE)
TOT2019$new_mw <- TOT2019$value - TOT2019$mw
TOT2019 <- TOT2019[, c("year", "new_mw")]
colnames(TOT2019)[which(names(TOT2019)=="new_mw")] <- "mw"

fall_cap$sector <- 'Actual'
TOT2019$sector <- 'Expected'
fall_cap <- merge(fall_cap, TOT2019, all=TRUE)
fall_cap

fall_prices <- eia_plus %>% filter(sheet == 'Average System Price' & sector == 'Blended')
fall_prices <- dplyr::filter(fall_prices, grepl('Q4|2019Q2', time_period))
fall_prices <- fall_prices[cols_to_keep]
colnames(fall_prices)[which(names(fall_prices)=='value')] <- "price"
fall_prices
fall_cap
#fall_prices_data <- merge(fall_cap, fall_prices, by="year")
fall_prices_data <- merge(fall_prices, fall_cap, by="year") %>% distinct()
#fall_prices_data <- left_join(fall_prices, fall_cap, by="year")
#fall_prices_data <- rbind(fall_cap, fall_prices)
fall_prices_data
fall_prices_data$newsector <- ifelse(fall_prices_data$sector == 'Actual', 'Through Q2 2019', ifelse(fall_prices_data$sector == 'Expected', 'Forecast', 'Error'))
fall_prices_data
fall_chart <- ggplot(fall_prices_data) + 
              #geom_bar(aes(x=factor(year), y=mw, fill=forcats::fct_rev(factor(sector, levels=c("Actual", "Expected"))), alpha=sector), stat="identity", width=.6) + 
              geom_bar(aes(x=factor(year), y=mw, fill=forcats::fct_rev(factor(newsector, levels=c("Through Q2 2019", "Forecast")))), stat="identity", width=.6) + 
              #geom_line(aes(x=factor(year), y=price*2814, group=1), stat="identity", color="#ffe14f", size=1.5) + 
              geom_smooth(aes(x=factor(year), y=price*2814, group=1), stat="identity", color="#ffe14f", size=1.5) + 
              scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), name="Capacity (MWdc)", label=comma, sec.axis = sec_axis(~./2814, labels=function(price) paste0("$",price,".00"), name="Blended Average PV System Price ($/Watt)")) + 
              #scale_fill_manual(values=c(alpha("#2f70af", 0.25), "#2f70af"), labels=c("Expected", "Actual")) +
              scale_fill_manual(values=c(alpha("#2f70af", 0.25), "#2f70af"), labels=c("Forecast", "Through Q2 2019")) +
              scale_x_discrete(labels=c("2019"="Q2 2019"))+
              seia_style() +  
              #scale_alpha_manual(values = c("Actual"=1, "Expected"=0.25), guide=FALSE) + 
              scale_alpha_manual(values = c("Through Q2 2019"=1, "Forecast"=0.25), guide=FALSE) + 
              #scale_alpha_manual(values = c("Actual"=1, "Expected"=0.25)) + 
              labs(title='U.S. Solar PV Price Declines and Deployment Growth') +
              guides(fill=guide_legend(reverse=TRUE)) +
              theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20)))+
              #guides(fill=FALSE)+
              transition_reveal(as.numeric(year))

#animate(fall_chart, nframes=750, fps=25, end_pause=50, width=1872, height=720)
animate(fall_chart, fps=60, end_pause=50, width=1872, height=720)
#animate(fall_chart, renderer=gifski_renderer(), end_pause=50, width=18.72, height=720)
anim_save("falling_prices_4.gif", animation = last_animation(), path = "C:/tmp/industry trends/")


fall_chart
ggsave("C:/tmp/industry trends/falling_price2.png", width=18.72, height=7.2, dpi=500)


###############################
# FERC Annual Additions of Electric Capacity
# PPT #5
ferc_data <- eia_plus %>% filter(sheet=='FERC New Infrastructure' & !(year=='Pre 2010 Cumulative') & time_period_type == 'Annual')

ferc_main_types <- ferc_data %>% filter(type %in% c("Solar", "Natural Gas", "Coal", "Wind"))
cols_to_keep <- c("year", "type", "value")
ferc_main_types <- ferc_main_types[cols_to_keep]
ferc_main_types

ferc_other_types <- ferc_data %>% filter(!(type %in% c("Solar", "Natural Gas", "Coal", "Wind")))
ferc_others <- ferc_other_types %>%
                group_by(year) %>%
                summarise(value=sum(value))
ferc_others$type <- 'Other'
ferc_others

ferc_chart_data <- merge(ferc_main_types, ferc_others, all=TRUE)
ferc_chart_data <- ferc_chart_data %>% filter(year != '2010Pre')
ferc_chart_data
cols_to_keep <- c("year", "type", "value")
ferc_chart_data <- ferc_chart_data[cols_to_keep]

type_order <- c("Solar", "Natural Gas", "Coal", "Wind", "Other")
ferc_chart_data <- ferc_chart_data[order(match(ferc_chart_data$type, type_order)),]

year_order <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
ferc_chart_data <- ferc_chart_data[order(match(ferc_chart_data$year, year_order)),]

ferc_chart_data


ferc_labels <- ferc_chart_data %>%
                group_by(year) %>%
                mutate(percent=value/sum(value),
                       pos=cumsum(value) - .5*value)


ferc_chart <- ggplot(ferc_chart_data) + 
              geom_bar(aes(x=year, y=value, fill=forcats::fct_rev(factor(type, levels=c("Solar", "Natural Gas", "Coal", "Wind", "Other")))), position="fill", stat="identity", width=0.75) + 
              geom_text(data=ferc_labels, aes(x=year, y=percent, label=paste(round(percent*100, 0), "%")), position=position_fill(vjust=0.5)) +
              labs(y="Percent of Total Capacity Additions", x="", title="Annual New Electric Capacity Additions") + 
              scale_x_discrete(labels=c("2019"="Q2 2019"))+
              seia_style() +
              scale_fill_manual(values=c("#ad6eae", "#88b551", "#37b3e5", "#f78237", "#ffe14f")) +
              guides(fill=guide_legend(reverse=TRUE)) + 
              scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), labels=scales::percent, breaks=seq(0,1,.1))
  
ferc_chart
ggsave("C:/tmp/industry trends/ferc.png", width=18.72, height=7.2, dpi=300)

#############################
# Blurbs
# PPT #6

#############################
# Soft Costs
# PPT #7
# NEEDS SOFT COSTS AS % of TOTAL COST
resi_component_data <- eia_plus %>% filter(sheet=='National PV Price Breakdowns' & sector=='Residential' & time_period %in% c("2016Q1", "2016Q2", "2016Q3", "2016Q4", "2017Q1", "2017Q2", "2017Q3", "2017Q4", "2018Q1", "2018Q2", "2018Q3", "2018Q4", "2019Q1"))
total_prices <- resi_component_data %>% filter(type=='Turnkey Pricing')
rest_prices <- resi_component_data %>% filter(type!='Turnkey Pricing')
cols_to_keep <- c("time_period", "value", "type")
total_prices <- total_prices[cols_to_keep]
rest_prices <- rest_prices[cols_to_keep]
soft_costs <- rest_prices %>%  
                filter(type %in% c("Design, Engineering, Permitting", "Direct Labor", "Supply Chain, Overhead, Margin")) %>%
                group_by(time_period) %>%
                summarise(soft_costs=sum(value))
soft_costs <- merge(soft_costs, total_prices, by="time_period")
#soft_costs
soft_costs$ratio <- soft_costs$soft_costs/soft_costs$value
soft_costs
soft_costs$check <- soft_costs$ratio * soft_costs$value
soft_costs
soft_costs <- soft_costs[c("time_period", "ratio")]
soft_costs
chart_prices <- merge(rest_prices, soft_costs, all=TRUE)
chart_prices
resi_chart <- ggplot(chart_prices) + 
              geom_bar(aes(x=time_period, y=value, fill=type), stat="identity") + 
              geom_line(aes(x=factor(time_period), y=sum(value)*ratio/10, group=type), stat="identity", color="#ffe14f", size=1.5) + 
              scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), name="$/Watt", label=comma, sec.axis = sec_axis(~., labels=function(ratio) paste0(ratio,"%"), name="Soft Costs as % of Total")) + 
              seia_style()


resi_chart

##############################
# Utility Impacts by Tariffs
# PPT #8
utility_component_data <- eia_plus %>% filter(sheet=='National PV Price Breakdowns' & sector=='Utility: Fixed-Tilt' & type!='Turnkey Pricing')
utility_chart <- ggplot(utility_component_data) + 
                 geom_bar(aes(x=time_period, y=value, fill=type), stat="identity") + 
                 seia_style()
utility_chart

##############################
# 50 State Market
# PPT #9
# SEE TABLEAU, but could use R+Leaflet/Mapbox

##############################
# Resi Market - CA, Next 9 States, then Rest
# PPT #10
resi_data <- eia_plus %>% filter(sector=="Residential" & sheet=="Capacity - Current Installs" & year != '2010Pre')
resi_data

# Get Top 10
resi_ranks <- resi_data %>%
                group_by(state_full) %>%
                summarise(sumvalue=sum(value)) %>% 
                mutate(ranks=order(order(sumvalue, decreasing=TRUE)))
resi_ranks

first_state <- resi_ranks %>% filter(ranks==1)
first_state_name <- first_state$state_full
first_data <- resi_data %>% 
              filter(state_full==first_state_name) %>%
              group_by(state_full, year) %>%
              summarise(value=sum(value))
first_data$group <- first_state_name
first_data

next_9_states <- resi_ranks %>% filter(ranks > 1 & ranks <= 10)
next_9_names <- next_9_states$state_full
next_9_data <- resi_data %>%
               filter(state_full %in% c(next_9_names)) %>%
               group_by(year) %>%
               summarise(value=sum(value))
next_9_data$group <- "Next 9 States"
next_9_data

other_ranks <- resi_ranks %>% filter(ranks>10)
other_names <- other_ranks$state_full
other_data <- resi_data %>%
              filter(state_full %in% c(other_names)) %>%
              group_by(year) %>%
              summarise(value=sum(value))
other_data$group <- "Other"
other_data

tmp_data <- merge(first_data, next_9_data, all=TRUE)
chart_data <- merge(tmp_data, other_data, all=TRUE)
chart_data <- chart_data %>% filter(year!='2010PreCumulative')

resi_chart <- ggplot(chart_data) + 
              geom_bar(aes(x=year, y=value, fill=forcats::fct_rev(group)), width=.75, stat="identity") + 
              seia_style() + 
              labs(y="Capacity (MWdc)", x="", title="Residential Solar PV Installations") + 
              scale_fill_manual(values=c("#37b3e5", "#ffe14f", "#2f70af")) + 
              scale_y_continuous(expand=expand_scale(mult=c(0,0.02))) +
              guides(fill=guide_legend(reverse=TRUE))+
              scale_x_discrete(labels=c("2019"="Q2 2019"))

resi_chart
ggsave("C:/tmp/industry trends/resi_state_comparison.png", width=18.72, height=7.2, dpi=300)

########################################
# Non-Resi, Community vs. Rest of Sector
# PPT #11
# Get total Non-Residential data by quarter
nonresi_qdata <- eia_plus %>% filter(sheet=='Capacity - Current Installs' & sector=='Non-Residential')
# Aggregate quarterly data to annual sums
nonresi_data <- nonresi_qdata %>%
                  group_by(year) %>%
                  summarise(total_value=sum(value))

# Get total Community Solar data by quarter
commsolar_qdata <- eia_plus %>% filter(sheet=='Capacity - Community Solar')
# Aggregate quarterly data to annual sums
commsolar_data <- commsolar_qdata %>%
                    group_by(year) %>%
                    summarise(cs_value=sum(value))

# Merge the two dataframes
merge_data <- merge(nonresi_data, commsolar_data, by="year")
# Subtract community solar from total Non-Residential annual sums
merge_data$nr_value <- merge_data$total_value - merge_data$cs_value

# Re-separate data and rename to fit into final frame
nr_data <- merge_data[,c("year", "nr_value")]
nr_data$sector <- "C&I, Non-Profit, Gov't"
colnames(nr_data) <- c("year", "value", "sector")

# Re-separate data and rename to fit into final frame
cs_data <- merge_data[,c("year", "cs_value")]
cs_data$sector <- "Community Solar"
colnames(cs_data) <- c("year", "value", "sector")

# Merge CS and NR-CS frames into final dataframe
chart_data <- merge(nr_data, cs_data, all=TRUE)
chart_data
# Create the chart
nonresi_chart <- ggplot(chart_data, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("C&I, Non-Profit, Gov't", "Community Solar"))))) + 
                 geom_bar(stat='identity', width=0.5) + 
                 labs(y="Capacity (MWdc)", x="", title="Non-Residential Solar PV Installations") + 
                 seia_style() + 
                 scale_fill_manual(values=c("#ffe14f", "#2f70af")) + 
                 scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
                 guides(fill=guide_legend(reverse=TRUE))+
                 scale_x_discrete(labels=c("2019"="Q2 2019"))

nonresi_chart
# Save the chart
ggsave("C:/tmp/industry trends/comm_solar.png", width=18.72, height=7.2, dpi=300)

##########################
# Utility Project Pipeline
# PPT #12


##########################
# Solar PV Deployment
# PPT #13
smi_histfcast <- eia_plus %>% filter((time_period != '2010PreCumulative' & time_period != '2019') & (sheet=='Capacity - PV Forecast') & (sector %in% c('Residential', 'Non-Residential', 'Utility')))
smi_us <- smi_histfcast %>% 
  group_by(sector, time_period) %>%
  summarise(value=sum(value))

growth_chart <- ggplot(smi_us, aes(x=factor(time_period), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility"))))) + 
                geom_bar(stat='identity', width=0.5) + 
                labs(y="Capacity (MWdc)", x="", title="US Solar PV Deployment Forecast", caption="Source: SEIA/Wood Mackenzie - Solar Market Insight") + 
                seia_style() + 
                scale_fill_manual(values=c("#37b3e5", "#ffe14f", "#2f70af")) + 
                scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + 
                guides(fill=guide_legend(reverse=TRUE)) +
                scale_x_discrete(labels=c("2019"="2019E", "2020"="2020E", "2021"="2021E", "2022"="2022E", "2023"="2023E", "2024"="2024E"))+
                transition_reveal(as.numeric(time_period))
#+
#                transition_reveal(as.numeric(time_period))

growth_chart


#animate(growth_chart, nframes=750, fps=25, end_pause=50, width=1200, height=900)
#animate(growth_chart, nframes=750, end_pause=50, options(ani.width=1200, ani.height=900))
animate(growth_chart, renderer=gifski_renderer(), end_pause=50, width=1872, height=720)

anim_save("forecast_2.gif", animation = last_animation(), path = "C:/tmp/industry trends/")  


ggsave("C:/tmp/industry trends/growth_chart.png", width=18.72, height=7.2, dpi=100)

