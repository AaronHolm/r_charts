
library(scales)
library(janitor)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(forcats)

seia_style <- function() {
       font <- "Roboto Black"
       
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
               plot.caption = ggplot2::element_blank(), plot.margin = unit(c(.5,2.5,.5, .5), "cm"),
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

smi_noncsp_annual <- read_csv("C:/tmp/smi_noncsp.csv")
noncsp_states <- unique(smi_noncsp_annual$state_full)
for(state in noncsp_states){
  state_annual <- smi_noncsp_annual %>% filter(state_full==state)
  state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility"))))) + geom_bar(stat='identity', width=0.5) + labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + seia_style() + scale_fill_manual(values=c("#37b3e5", "#ffe14f", "#2f70af")) + scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + guides(fill=guide_legend(reverse=TRUE))
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

smi_fcast <- read_csv("C:/tmp/smi_fcast.csv")
fcast_states <- unique(smi_fcast$state_full)

for(state in fcast_states){
  state_annual <- smi_fcast %>% filter(state_full==state)
  if (state %in% csp_states){
    state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility PV", "Utility CSP"))))) + geom_bar(stat='identity', width=0.5) + labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + seia_style() + scale_fill_manual(values=c("#88b551", "#37b3e5", "#ffe14f", "#2f70af")) + scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + guides(fill=guide_legend(reverse=TRUE))
  }
  else{
    state_chart <- ggplot(state_annual, aes(x=factor(year), y=value, fill=forcats::fct_rev(factor(sector, levels=c("Residential", "Non-Residential", "Utility"))))) + geom_bar(stat='identity', width=0.5) + labs(y="Capacity (MW)", x="", title=paste(state,"Annual Solar Installations")) + seia_style() + scale_fill_manual(values=c("#37b3e5", "#ffe14f", "#2f70af")) + scale_y_continuous(expand=expand_scale(mult=c(0,0.02)), label=comma) + guides(fill=guide_legend(reverse=TRUE))
  }
  ggsave(paste("C:/tmp/factsheets/charts/forecast/",state,"_fcast.png", sep=""), width=18.72, height=7.2, dpi=100)
}