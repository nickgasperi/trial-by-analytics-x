# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load 2020s nfl data
nfldata20 = load_pbp(2020:2025)

# create two tibbles - one for passing and another for rushing - will join tibbles later
# create passing tibble first, as this will define the group that will appear on the plot

# tibble 1 - passing
qb_pass_data = nfldata20 %>%
  filter(!is.na(passer_player_id),
         !is.na(yards_gained),
         season_type == "REG") %>%
  group_by(season,
           posteam,
           passer_player_id,
           passer_player_name) %>%
  summarize(att_p = n(),
            pass_yds = sum(yards_gained),
            ypa = pass_yds/att_p,
            games = n_distinct(game_id),
            pass_yd_per_game = pass_yds/games) %>%
  filter(att_p > 350) %>%
  arrange(-pass_yd_per_game) %>%
  print(n = 25)

# transform tibble 1 to include altered col headers for matching in future join function
# also drop extra columns by selecting only necessary columns
qb_pass_join = qb_pass_data %>%
  select(season,
         posteam,
         passer_player_id,
         passer_player_name,
         pass_yd_per_game) %>%
  rename(player_id = passer_player_id,
         player_name = passer_player_name) %>%
  print(n = 5)

# tibble 2 - rushing
qb_rush_data = nfldata20 %>%
  filter(!is.na(rusher_player_id),
         season_type == "REG") %>%
  group_by(season,
           rusher_player_id,
           rusher_player_name) %>%
  summarize(att_r = n(),
            rush_yds = sum(yards_gained),
            ypc = rush_yds/att_r,
            games = n_distinct(game_id),
            rush_yd_per_game = rush_yds/games) %>%
  arrange(rush_yd_per_game) %>%
  print(n = 25)

# transform tibble 2 - match col headers to tibble 1
# also drop extra columns by selecting only necessary columns
qb_rush_join = qb_rush_data %>%
  select(season,
         rusher_player_id,
         rusher_player_name,
         rush_yd_per_game) %>%
  rename(player_id = rusher_player_id,
         player_name = rusher_player_name) %>%
  print(n = 5)

# join tibbles
all_qb_data = qb_pass_join %>%
  left_join(qb_rush_join,
            by = c("season",
                   "player_id",
                   "player_name"))

# filter newly joined tibble to only include qbs with >=260 pass yd/game
all_qb_data_a = all_qb_data %>%
  filter(pass_yd_per_game >= 260) %>%
  print(n = Inf)

# add extra column for plot label
all_qb_data_a$label_id = paste(all_qb_data_a$player_name,
                               all_qb_data_a$season,
                              sep = ", ")

# plot data
all_qb_plot = ggplot(data = all_qb_data_a,
                     aes(x = rush_yd_per_game,
                         y = pass_yd_per_game)) +
  geom_hline(yintercept = mean(all_qb_data_a$pass_yd_per_game),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.6) +
  geom_vline(xintercept = mean(all_qb_data_a$rush_yd_per_game),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.6) +
  geom_point(aes(color = posteam)) +
  scale_color_nfl(type = "primary") +
  geom_text_repel(box.padding = 0.3,
                  aes(label = label_id,
                      color = posteam)) +
  labs(title = "Single-Season Pass Yards per Game Leaders - Rushing vs. Passing",
       subtitle = "2020-2025 NFL Reg. Seasons (thru '25 Wk 11) | min. 351 pass att.",
       x = "Rush Yd./Game",
       y = "Pass Yd./Game",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold",
                                  size = 18),
        plot.subtitle = element_text(face = "bold",
                                     size = 16),
        plot.caption = element_text(size = 12),
        axis.title = element_text(face = "bold",
                                  size = 13),
        axis.text = element_text(size = 12))

# view plot
all_qb_plot

# save plot to local files
ggsave("X post 70 - qb_rush_pass.png",
       width = 10.5, height = 7,
       dpi = "retina")

