#load packages
library(tidyverse)
library(dplyr)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggplot2)

# load NFL data from 2018 to 2024
nfldata81 = load_pbp(2018:2024)

# filter data to include only passes thrown by Mahomes
# 
mahomesallpass = pbpallpass %>%
  filter(  "P.Mahomes",
         play_type == "pass",
         !is.na(air_yards)) %>%
  group_by(passer_player_id, passer_player_name, season, posteam) %>%
  summarize(ratiotdint = (sum(touchdown)/sum(interception)),
            td = sum(touchdown),
            int = sum(interception),
            .groups = "drop") %>%
    print(n = Inf)

            
            
## use ggplot to create plot
## use geom_nfl_headshots to add player image to each data point
## define color as red within geom_line to match team color
## use theme() command to format plot title elements
mahomesratioplot = ggplot(data = mahomesallpass, 
                          aes(x = season, y = ratiotdint))+
  geom_nfl_headshots(aes(player_gsis = "00-0033873"), height = .1)+
  labs(title = "Mahomes Touchdown to Interception Ratio By Year",
       subtitle = "2018 - 2024",
       x = "Season",
       y = "TD:Int Ratio")+
  geom_line(color = "red")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
        plot.subtitle = element_text(hjust = 0.5, size = 12))
## view the plot
mahomesratioplot



