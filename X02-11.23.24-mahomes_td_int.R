#load packages
library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflfastR)
library(nflplotR)

# load NFL data from 2018 to 2024 seasons
nfldata15 = load_pbp(2018:2024)

# wrangle data into new tibble
mahomesdata1 = nfldata15 %>%
  filter(passer_player_name == "P.Mahomes",
         play_type == "pass",
         !is.na(air_yards)) %>% 
  group_by(passer_player_id,
           passer_player_name,
           season,
           posteam) %>%
  summarize(att = n(),
            td = sum(touchdown),
            int = sum(interception),
            tdint = td/int) %>%                   # create TD:Int Ratio for plotting
  mutate(tdint = round(tdint, digits = 1)) %>%
  print(n = Inf)                                    

# plot Mahomes TD:Int Ratio by Season
mahomesratioplot = ggplot(data = mahomesdata1, 
                          aes(x = season, y = tdint)) +
  labs(title = "Mahomes Touchdown to Interception Ratio By Year",
       subtitle = "Regular Season & Postseason",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Season", y = "TD:Int Ratio",
       tag = "00-0033873") +                  # declaring tag to be used when adding player image in theme()
  geom_line(aes(color = posteam),
            linewidth = 1.3) +
  geom_point(aes(color = posteam),
             size = 3) +
  scale_color_nfl(type = "primary") +         # apply primary team color for geom_line() and geom_point()
  scale_x_continuous(n.breaks = 7) +          # specify number of labels listed on x axis to make sure all seasons are included
  theme_minimal()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.tag = element_nfl_headshot(size = 5,
                                        hjust = 1,
                                        vjust = 1),
        plot.tag.position = c(1,1),                  # inserts player image
        plot.title = element_text(face = "bold",
                                  size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold",
                                  size = 14),
        axis.text = element_text(size = 14))

# view plot
mahomesratioplot

# save plot to local files
ggsave("X post 2 - mahomes_tdint.png",
       width = 10.5, height = 7,
       dpi = "retina")