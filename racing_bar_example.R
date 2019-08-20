library(imager)
library(ggimage)
library(magick)
library(tidyverse)
library(gganimate)
library(png)
library(gapminder)
library(ggimage)
library(rsvg)


#read data
df <- data.frame(Player = rep(c("Florida", "Georgia", "North Carolina", "South Carolina"), 6),
                 Team = rep(c("ManCity", "Liverpool", "Arsenal", "Tottenham"), 6), 
                 Gameday = c(2020,2020,2020,2020,2021,2021,2021,2021,2022,2022,2022,2022,2023,2023,2023,2023,2024,2024,2024,2024,2025,2025,2025,2025),
                 Goals = c(0,1,2,0,1,1,3,1,2,1,3,2,2,2,4,3,3,2,4,5,5,3,5,6),
                 stringsAsFactors = F)

# import images
df2 <- data.frame(Player = c("Florida", "Georgia", "North Carolina", "South Carolina"),
                  Image = sample(c("C:/tmp/R/state_flags/Flag_of_Florida.svg", 
                                   "C:/tmp/R/state_flags/Flag_of_Georgia.svg",
                                   "C:/tmp/R/state_flags/Flag_of_North_Carolina.png",
                                   "C:/tmp/R/state_flags/Flag_of_South_Carolina.svg")),
                  stringsAsFactors = F)

gap <- df %>%
  group_by(Gameday) %>%
  mutate(rank = min_rank(-Goals) * 1,
         Value_rel = Goals/Goals[rank==1],
         Value_lbl = paste0(" ", Goals)) %>%
  filter(rank <=10) %>%
  ungroup()

gap %>%
  left_join(df2, by = "Player") %>% # add image file location to the dataframe being
  # passed to ggplot()      
  group_by(Player) %>%
  arrange(Gameday) %>%
  mutate(prev.rank = lag(rank)) %>%
  ungroup() %>%      
  group_by(Gameday) %>%
  arrange(rank, prev.rank) %>%
  mutate(x = seq(1, n())) %>%
  ungroup() %>%
  
  ggplot(aes(x = x, y = Goals, fill = Player, color = Player)) +
  geom_col() +
  geom_text(aes(y = 0, label = Player), size = 5, color="black", hjust = -0.05) +
  geom_text(aes(label = Value_lbl), hjust = 0) +
  
  geom_image(aes(x = x, image = Image), y = 0,  # add geom_image layer
             size = 0.25, hjust = 1,
             inherit.aes = FALSE) +
  
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(title = "Gameday: {closest_state}", x = "", y = "Goals scored") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0, size = 26),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        plot.margin = margin(1, 1, 1, 4, "cm")) +
  transition_states(Gameday, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')



flag_links <- function(x) {
  base_link <- "https://en.wikipedia.org/wiki/Flags_of_the_U.S._states_and_territories/media/File:Flag_of_"
  states <- c("North_Carolina", "Florida", "Georgia", "South_Carolina", "Alabama", "Tennessee", "Arkansas", "Kentucky", "West_Virginia")
  state_flag_links <- list()
  file_type <- ".svg"
  for(state %in% states){
    flag_link <- paste(base_link, state, file_type, sep="")
  }
}

