# load packages
library(tidyverse)      # data wrangling
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggrepel)        # replaces geom_text

# load NFL data from 2024 season
nfldata = load_pbp(2024)

# filter long pass data
# calc cpoe and epa per attempt
# wrangle data into new tibble
deepdata1 = nfldata %>%
  filter(week < 18,
         play_type == "pass",
         air_yards >= 20,
         !is.na(cpoe),
         !is.na(passer_player_id),
         !is.na(air_yards)) %>%
  group_by(passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(longatt = n(),
            cpoe = mean(cpoe),
            epa = sum(epa)/sum(longatt),
            .groups = "drop") %>%
  filter(longatt >= 25) %>%
  arrange(-longatt) %>%
  print(n = Inf)

# create a second tibble to calculate total attempts
regdata1 = nfldata %>%
  filter(week < 18,
         qb_spike == 0,
         !is.na(passer_player_id),
         !is.na(air_yards)) %>%
  group_by(passer_player_id) %>%
  summarize(att = n()) %>%
  filter(att > 200) %>%
  arrange(-att) %>%
  print(n = Inf)

# join the two tibbles to add the total attempts column to the original long pass table
passing55 = deepdata1 %>%
  left_join(regdata1, by = "passer_player_id") %>%
  print(n = Inf)

# plot data
deepplot1 = ggplot(data = deepdata1, aes(x = cpoe, y = epa)) +
  geom_hline(yintercept = mean(passing55$epa), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(passing55$cpoe), linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_point(aes(color = posteam, size = longatt, alpha = 0.75)) +
  geom_text_repel(aes(label = passer_player_name, fontface = "bold", color = posteam)) +
  scale_color_nfl(type = "primary") +
  labs(title = "CPOE vs. EPA Per Pass Attempt | 20+ Air Yards",
       subtitle = "2024 NFL Wk 1-17 (min. 25 att.)",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "CPOE", y = "EPA") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 19),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 17),
        plot.caption = element_text(size = 10),
        axis.title = element_text(face = "bold.italic", size = 13),
        axis.text = element_text(size = 13))

# view plot
deepplot1

# save plot
ggsave("X post 23 - deep_passes.png",
       width = 10.5, height = 7, dpi = "retina")
