# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load 2024 NFL data
nfldata4 = load_pbp(2024)

# filter data
gimmick4 = nfldata4 %>%
  filter(week < 17,
         play_type == "pass",
         !is.na(yards_gained),
         !is.na(air_yards),
         !is.na(epa),
         !is.na(passer_player_id)) %>%
  group_by(passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(att = n(),
            epa = sum(epa)/sum(att),
            airyd = sum(air_yards)/sum(att)) %>%
  filter(att < 3) %>%
  arrange(-att) %>%
  print(n = Inf)

# plot data
gim4plot = ggplot(data = gimmick4,
                  aes(x = airyd, y = epa)) +
  geom_hline(yintercept = mean(gimmick4$epa),
             linetype = "dashed",
             color = "black") +
  geom_vline(xintercept = mean(gimmick4$att),
             linetype = "dashed",
             color = "black") +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.06,
                 alpha = 0.7) +
  geom_text_repel(box.padding = 0.8,
                  aes(label = gimmick4$passer_player_name,
                      fontface = "bold",
                      size = 12)) +
  labs(title = "Air Yards vs. EPA Per Attempt",
       subtitle = "2024 NFL Wk 1-16 (1 or 2 att.)",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Air Yards",
       y = "EPA") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 22),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 20),
        plot.caption = element_text(size = 13),
        axis.title = element_text(face = "bold.italic",
                                  size = 15),
        axis.text = element_text(size = 15))

# view plot
gim4plot

# save plot to local files
ggsave("X post 21 - gimmick_passing.png",
       width = 14, height = 10,
       dpi = "retina")  
