#load packages
library(tidyverse)
library(dplyr)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggplot2)
library(ggrepel)

# load NFL data from 2018 to 2024
nfldata15 = load_pbp(2018:2024)

# filter data to include only passes thrown by Mahomes
# summarize td, int, and ratio by year
mahomesdata1 = nfldata15 %>%
  filter(passer_player_name == "P.Mahomes",
         play_type == "pass",
         !is.na(air_yards)) %>%
  group_by(passer_player_id, passer_player_name, season, posteam) %>%
  summarize(att = n(),
            td = sum(touchdown),
            int = sum(interception),
            tdint = td/int) %>%
  mutate(tdint = round(tdint, digits = 1)) %>%
  print(n = Inf)

## use geom_nfl_headshots to add player image to each data point
## define color as red within geom_line to match team color
## use theme() command to format plot title elements
mahomesratioplot = ggplot(data = mahomesdata1, 
                          aes(x = season, y = tdint))+
  labs(title = "Mahomes Touchdown to Interception Ratio By Year",
       subtitle = "Regular & Postseason",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Season", y = "TD:Int Ratio",
       tag = "00-0033873")+
  geom_line(aes(color = posteam), linewidth = 1.3)+
  geom_point(aes(color = posteam), size = 3) +
  scale_color_nfl(type = "primary") +
  scale_x_continuous(n.breaks = 7) +
  theme_minimal()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.tag = element_nfl_headshot(size = 4.75, hjust = 1, vjust = 1),
        plot.tag.position = c(1,1),
        plot.title = element_text(face = "bold", hjust = 0, size = 18),
        plot.subtitle = element_text(hjust = 0, size = 16),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 14))

# view plot
mahomesratioplot

# save plot
ggsave("X post 2 - mahomes_tdint.png",
       width = 10.5, height = 7, dpi = "retina")
