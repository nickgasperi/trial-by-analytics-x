# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
nfldata = load_pbp(2024)

# filter data
afcte = nfldata %>%
  filter(receiver_player_id == "00-0030506" | receiver_player_id == "00-0033858",
         !is.na(yards_gained),
         !is.na(epa)) %>%
  group_by(play_id,
           receiver_player_id,
           receiver_player_name,
           posteam) %>%
  summarize(targets = n(),
            epa = sum(epa),
            yards = sum(yards_gained),
            .groups = "drop") %>%
  print(n = 10)

# add columns for cumulative yards, epa, and targets
afcte$cumyds = ave(afcte$yards, afcte$receiver_player_id, FUN = cumsum)
afcte$cumepa = ave(afcte$epa, afcte$receiver_player_id, FUN = cumsum)
afcte$cumtgts = ave(afcte$targets, afcte$receiver_player_id, FUN = cumsum)

# view new tibble
afcte %>%
  print(width = Inf)

# frame last targets for later geom_point
frame4 = afcte %>%
  filter(play_id == "4755" | play_id == "4483") %>%
  print(n = Inf)

# create plot 1
plotafc1 = ggplot(data = afcte,
                  aes(x = cumtgts, y = cumyds)) +
  geom_line(aes(color = posteam)) +
  geom_point(data = frame4,
             aes(color = posteam)) +
  geom_text_repel(data = frame4,
                  aes(label = receiver_player_name,
                      fontface = "bold.italic",
                      color = posteam,
                      size = 3)) +
  scale_color_nfl(type = "primary") +
  labs(title = "Travis Kelce vs. Jonnu Smith - Cumulative Receiving Yards",
       subtitle = "2024 NFL Wk 1-17",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Targets",
       y = "Yards") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 19),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 17),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold",
                                  size = 13),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_blank())

# view plot 1
plotafc1

# save plot 1 to local files
ggsave("X post 26.1 - afc_tightends.png",
       width = 10.5, height = 7,
       dpi = "retina")

# to create plot 2 - switch yards with epa
# rename axis and title
# renmae plot 'afcte2'
# choose a new name for ggsave
