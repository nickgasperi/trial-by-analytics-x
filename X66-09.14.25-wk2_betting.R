# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024-2025 pbp data
nfldata_2425 = load_pbp(2024:2025)


# Plot 1 - James Cook Rush Attempts ---------------------------------------

# create tibble with james cook rushing attempts by week
jc_wk2_rush = nfldata_2425 %>%
  filter(rusher_player_id == "00-0037248",
         play_type == "run",
         posteam == "BUF") %>%
  group_by(season,
           game_id,
           week,
           rusher_player_id,
           rusher_player_name,
           posteam) %>%
  summarize(att = n(),
            .groups = "drop") %>%
  arrange(season,
          week) %>%
  print(n = Inf)

# combine year and week into one field for easier x axis sorting in plot
jc_wk2_rush_2 = jc_wk2_rush %>%
  mutate(season_week = paste0(season, "-W", week),
         season_week = factor(season_week, 
                              levels = unique(season_week))) %>%
  print(n = Inf)

# plot data
jc_wk2_plot = jc_wk2_rush_2 %>%
  mutate(color3 = ifelse(jc_wk2_rush_2$att >= 10, "green4", "red")) %>%
  ggplot(aes(x = season_week, y = att, group = 1)) +
  geom_col(aes(color = color3,
               fill = color3),
           alpha = 0.8) +
  geom_hline(yintercept = 10,
             linetype = "dashed",
             color = "red") +
  scale_color_identity() +
  scale_fill_identity() +
  labs(title = "James Cook Rushing Attempts by Game",
       subtitle = "2024-2025 | green >= 10 att.",
       x = "Week",
       y = "Rush Attempts",
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
        axis.text.y = element_text(size = 14),
        axis.text.x = element_blank())
  
# view plot
jc_wk2_plot

# save plot to local files
ggsave("X post 66.1 - james_cook_rush_att.png",
       width = 10.5, height = 7,
       dpi = "retina")

# Plot 2 - Breece Hall Rushing Yards ---------------------------------------

# create tibble with Breece Hall rush att by game
hall_rush_att = nfldata_2425 %>%
  filter(rusher_player_id == "00-0038120",
         play_type == "run",
         posteam == "NYJ") %>%
  group_by(season,
           game_id,
           week,
           rusher_player_id,
           rusher_player_name,
           posteam) %>%
  summarize(att = n(),
            .groups = "drop") %>%
  arrange(season,
          week) %>%
  print(n = Inf)

# combine year and week into one field for easier x axis sorting in plot
hall_rush_2 = hall_rush_att %>%
  mutate(season_week = paste0(season, "-W", week),
         season_week = factor(season_week, 
                              levels = unique(season_week))) %>%
  print(n = Inf)

# plot data
hall_rush_plot = hall_rush_2 %>%
  mutate(color3 = ifelse(hall_rush_2$att >= 10, "green4", "red")) %>%
  ggplot(aes(x = season_week, y = att, group = 1)) +
  geom_col(aes(color = color3,
               fill = color3),
           alpha = 0.8) +
  geom_hline(yintercept = 10,
             linetype = "dashed",
             color = "red") +
  scale_color_identity() +
  scale_fill_identity() +
  labs(title = "Breece Hall Rushing Attempts by Game",
       subtitle = "2024-2025 | green >= 10 att.",
       x = "Week",
       y = "Rush Attempts",
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
        axis.text.y = element_text(size = 14),
        axis.text.x = element_blank())

# view plot
hall_rush_plot

# save plot to local files
ggsave("X post 66.2 - b_hall_rush_att.png",
       width = 10.5, height = 7,
       dpi = "retina")
