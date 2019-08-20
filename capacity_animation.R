library(scales)
library(janitor)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(forcats)
library(ggstance)

seia_style <- function() {
  font <- "Roboto Black"
  
  seia_colors <- c("#2f70af", "#88b551", "#ffffff", "#ffe14f", "#f78237", "#ad6eae", 
                   "#37b3e5", "#f78237", "#88b551", "#2f70af", "#ad6eae", "#ffffff", 
                   "#ffe14f", "#f78237", "#ad6eae")
  
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
    plot.caption = ggplot2::element_text(hjust=0), 
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


eia_plus <- read_csv("C:/tmp/eia_plus.csv")

cap_data <- eia_plus %>%
              filter(sheet=='Capacity - Current Installs' & year != '2010PreCumulative') %>%
              group_by(state_abbr, year) %>%
              summarise(value=sum(value))


cap_data$year <- as.numeric(cap_data$year)
cap_data

southeast_states <- c("FL", "SC", "NC", "GA", "AL", "TN", "KY", "LA", "VA", "AR", "MS", "WV")

cap_data <- cap_data %>% filter(state_abbr %in% southeast_states)


all_years = unique(cap_data$year)




table <- cap_data %>%
          filter(year == 2010) %>%
          group_by(state_abbr) %>%
          summarise(value=sum(value)) %>%
          mutate(year=2010) %>%
          select(state_abbr, year, value)
          
#ranked_cap <- data.frame(state_abbr=character(), year=character(), value=double(), rank=integer())

for(yr in all_years){
  table <- cap_data %>%  
                filter(year<=yr) %>%
                group_by(state_abbr) %>%
                summarise(value=sum(value)) %>%
                mutate(year=yr) %>%
                bind_rows(table)
  #year_data$rank <- rank(-year_data$value, na.last=TRUE, ties.method='min')
  #ranked_cap <- merge(ranked_cap, year_data, all=TRUE)
}

table
#ranked_cap

final_table <- table %>%
                group_by(year) %>%
                arrange(-value) %>%
                #mutate(rank=min_rank(-value),
                mutate(rank=row_number(),
                       value_rel = value/value[rank==1],
                       value_lbl = paste0(" ", round(value, 2))) %>%
                filter(rank<= 10) %>%
                ungroup()

final_table
write_csv(final_table, path="C:/tmp/final_anim_table4.csv")

#test_table <- read_csv("C:/tmp/final_anim_table3.csv")
#test_table <- test_table %>% filter(state_abbr %in% c("FL", "SC", "NC", "GA", "AL", "TN", "KY", "LA", "VA", "AR", "MS", "WV"))
#p <- test_table %>%
p <- final_table %>%
      ggplot(aes(x=-rank, y=value, group=state_abbr)) +
      geom_tile(aes(y=value/2, height=value, fill=state_abbr), width=0.9) +
      #geom_barh(stat="identity", aes(fill=state_abbr))+
      #geom_text(aes(label=state_abbr, hjust="right")) +
      #geom_text(aes(y=rank, label=state_abbr, hjust=0), size=12, family=font, colour="#1f1446") +
      geom_text(aes(y=-15, label=paste(state_abbr, " "), hjust=0), size=12, family=font, colour="#1f1446")+
      geom_text(aes(label=paste(scales::comma(value), "MW")), size=12, hjust="left", family=font, colour="#1f1446") +
      coord_flip(clip="off")+
      scale_x_continuous(" ", expand=expand_scale(mult=c(0,0.02)))+
      scale_y_continuous(labels=scales::comma, expand=expand_scale(mult=c(0,0.02)))+
      transition_time(year)+
      #transition_states(states=as.factor(year), transition_length=1, state_length=1)+
      ease_aes('linear', interval=0.01)+
      labs(title="State MW Race", 
           subtitle="MW in {round(frame_time,0)}",
           #subtitle="MW in {round(closest_state, 0)}",
           caption="Source: SMI") +
      seia_style()+
      guides(fill=FALSE, color=FALSE) +
      enter_fade()+
      exit_fade()+
      view_follow(fixed_y=FALSE, fixed_x=TRUE)+
      #view_follow(fixed_x=TRUE)+
      scale_fill_manual(values=seia_colors)+
      theme(axis.text.y.left=element_blank(),
            axis.text.y.right=element_blank(),
            axis.text.x.bottom=element_blank(),
            axis.text.x.top=element_blank())
      #      axis.text.x.top=geom_text(label=scales::comma(value)))
  
  
#animate(p, nframes=750, fps=25, end_pause=50, width=1200, height=900)
#animate(p, nframes=750, fps=30, renderer=gifski_renderer(), width=1200, height=600, end_pause=50)
#animate(p, nframes=750, fps=30, renderer=gifski_renderer(), width=1200, height=600, end_pause=50)
animate(p, renderer=gifski_renderer(), width=1200, height=600, end_pause=50)

anim_save("mw_race_by_state_SE_1.gif", animation = last_animation(), path = "C:/tmp/")

#p <- ggplot(final_table, aes(x=-rank,y = value, group = state_abbr)) +
#  geom_tile(aes(y = value / 2, height = value, fill = state_abbr), width = 0.9) +
#  geom_text(aes(y=0, label=paste(state_abbr, " ")), size=12, vjust=0.2, hjust=1) +
#  scale_y_continuous(labels=scales::comma) + 
#  scale_x_discrete("", position="top")+
#  coord_flip(clip="off") +
#  transition_time(year)+
#  guides(fill=FALSE)+
#  ease_aes('cubic-in-out') 

p

#view_follow(fixed_y=FALSE, fixed_x=TRUE)+
#geom_text(aes(y=value, label=value_lbl, hjust=0)) +
#transition_states(year, transition_length = 3, state_length = 2) +
#labs(title="Capacity (MW) by Rank and Year", subtitle="Year", caption="SEIA/Wood MacKenzie Solar Market Insight") +
#seia_style()



anim_p <- animate(p, nframes=750, fps=30, end_pause=50, width=1200, height=900, renderer=gifski_renderer("C:/tmp/cap_race.gif"))
