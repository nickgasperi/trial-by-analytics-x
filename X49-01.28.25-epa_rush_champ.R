# load packages
library(tidyverse)      # data wrangling
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load NFL data from 2024 season
nfldata = load_pbp(2024)

# wrangle data into new tibble
champdatarush = nfldata %>%
  filter(week == 21,
         qb_kneel == 0,
         sack == 0,
         !is.na(epa),
         !is.na(rusher_player_id)) %>%
  group_by(rusher_player_id,
           rusher_player_name,
           posteam) %>%
  summarize(rushes = n(),
            epaper = sum(epa)/sum(rushes)) %>%
  arrange(-rushes) %>%
  filter(rushes >= 8) %>%
  print(n = Inf)

# create plot
champplotrush = ggplot(data = champdatarush, aes(x = epaper,
                                                 xend = 0,
                                                 y = reorder(rusher_player_id, epaper),
                                                 yend = rusher_player_id)) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "darkgrey") +
  geom_segment(linewidth = 3.25,
               aes(color = posteam)) +
  scale_x_continuous(n.breaks = 6) +
  scale_color_nfl(type = "primary") +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.07) +
  labs(title = "2024 AFC/NFC Championship Rushing Efficiency | min. 8 att.",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "EPA/Rush") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold.italic",
                                  size = 18),
        plot.caption = element_text(size = 11),
        axis.text.y = element_nfl_headshot(size = 2.7),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold",
                                    size = 15),
        axis.text.x = element_text(size = 15))

# view plot
champplotrush

# save plot to local files
ggsave("X post 49 - epa_rush_champ_wknd.png",
       width = 10.5, height = 7,
       dpi = "retina")
