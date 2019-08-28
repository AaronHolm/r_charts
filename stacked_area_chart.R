library(ggplot2)
library(tidyverse)

# Get Data
pg = dbDriver("PostgreSQL")
con = dbConnect(pg, user=db_user, password=db_pass, host=db_host, port=db_port, dbname=db_name)
group_data = dbGetQuery(con, "select * from products.itc_2019")

# Transform the Data
basecase <- group_data %>% 
                   #filter(run_type=="O & M", scenario=="Basecase") %>%
                   filter(run_type=="Installers", scenario=="Basecase") %>%
                   group_by(state_full, year) %>%
                   summarise(MW=sum(MW/1000))
basecase <- basecase[c("state_full", "year", "MW")]

extension <- group_data %>%
                   filter(run_type=="Installers", scenario=="Extension") %>%
                   group_by(state_full, year) %>%
                   summarise(MW_ext=sum(MW/1000))
extension <- extension[c("state_full", "year", "MW_ext")]
extension <- merge(basecase, extension, by=c("state_full", "year"))
extension$MW <- extension$MW_ext - extension$MW
extension <- extension[c("state_full", "year", "MW")]
extension$scenario <- "Extension"
basecase$scenario <- "Basecase"
#cumulative_mw <- merge(basecase, extension, by=c("state_full", "year", "scenario", "MW"))
cumulative_mw <- bind_rows(basecase, extension)
#basecase = cumulative_mw %>% filter(scenario=="Basecase")
#extension = cumulative_mw %>% filter(scenario=="Extension")
# Make the Chart
states <- unique(cumulative_mw$state_full)
for(state in states){
  deployment_chart <- ggplot(cumulative_mw %>% filter(state_full==state), aes(x=factor(year), y=MW, fill=scenario, group=scenario))+
                      geom_area(position = position_stack(reverse = TRUE))+
                      scale_fill_manual(values=c("#37b3e5", "#1f1446"))+
                      seia_style()
  ggsave(paste("C:/tmp/ITC/charts/deployment/", state, "_incremental_deployment.png"), width=18, height=7, dpi=300)
  deployment_chart
}
