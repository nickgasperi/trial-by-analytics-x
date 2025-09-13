# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(ggrepel)

# load 2025 nfl data
nfldata25 = load_pbp(2025)

# create tibble for qb adot by game
qb_adot_data = nfldata25 %>%
  filter(play_type == "pass",
         qb_spike == 0,
         sack == 0,
         !is.na(passer_player_id),
         !is.na(air_yards)) %>%
  group_by(game_id,
           passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(att = n(),
            adot = mean(air_yards)) %>%
  filter(att >= 15) %>%
  print(n = Inf)

# create tibble for qb success rate
qb_succ_data = nfldata25 %>%
  filter(play_type == "pass",
         qb_spike == 0,
         sack == 0,
         !is.na(success),
         !is.na(passer_player_id)) %>%
  group_by(game_id,
           passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(att = n(),
            succ_rate = mean(success)) %>%
  filter(att >= 15) %>%
  print(n = Inf)

# join success rate column with original tibble
qb_tot_data = qb_adot_data %>%
  left_join(qb_succ_data,
            by = c("game_id", "passer_player_id", "passer_player_name",
                   "posteam")) %>%
  select(-att.x, -att.y)

# view new tibble
qb_tot_data %>%
  print(n = Inf)

# plot
qb_tot_plot = ggplot(data = qb_tot_data,
                      aes(x = adot, y = succ_rate)) +
  geom_hline(yintercept = mean(qb_tot_data$succ_rate),
             linetype = "dashed",
             color = "red",
             alpha = 0.6) +
  geom_vline(xintercept = mean(qb_tot_data$adot),
             linetype = "dashed",
             color = "red",
             alpha = 0.6) +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.03,
                 alpha = 0.8) +
  geom_text_repel(segment.color = NA,
                  aes(label = passer_player_name,
                      color = posteam)) +
  scale_color_nfl(type = "primary") +
  labs(title = "ADOT vs. Success Rate",
       subtitle = "2025 Wk1 + TNF Wk2 | Passing Plays",
       x = "ADOT",
       y = "Success Rate",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 16),
        plot.caption = element_text(size = 13),
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text = element_text(size = 14))

# view plot
qb_tot_plot

# save plot to local files
ggsave("X post 65 - qb_performance.png",
       width = 10.5, height = 7,
       dpi = "retina")  
  

