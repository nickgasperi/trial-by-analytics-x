# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggrepel)

# load data
data22 = load_pbp(2024)

# explore variable
data22 %>%
  pull(cpoe) %>%
  summary()

# filter data
datawk15 = data22 %>%
  filter(week == 15,
         play_type == "pass",
         !is.na(passer_player_id),
         !is.na(posteam),
         !is.na(air_yards),
         !is.na(cpoe)) %>%
  group_by(passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(att = n(),
            cpoe = mean(cpoe),
            airyd = mean(air_yards)) %>%
  filter(att > 3) %>%
  arrange(att) %>%
  print(n = Inf)

# plot data with ggplot
wk15passplot = ggplot(data = datawk15, aes(x = airyd, y = cpoe)) +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.038) +
  geom_text_repel(box.padding = 0.4,
                  aes(label = passer_player_name)) +
  geom_hline(yintercept = mean(datawk15$cpoe),
             linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(datawk15$airyd),
             linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  labs(title = "Air Yards vs. CPOE Per Attempt",
       subtitle = "2024 NFL Week 15 | Passing Plays (min. 3 att.)",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Air Yards",
       y = "CPOE") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 23),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 21),
        plot.caption = element_text(size = 13),
        plot.background = element_rect(fill = "#F0F0F0"),
        axis.title = element_text(face = "bold.italic", size = 15),
        axis.text = element_text(size = 13))

# view plot
wk15passplot

# save plot
ggsave("X post 12 - airyd_cpoe_wk15.png",
       width = 14, height = 10, dpi = "retina")
