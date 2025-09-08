# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(ggbeeswarm)
library(gt)
library(gtExtras)

# Plot 1 - Points Per Play by Season --------------------------------------

# load data from 2019-2024 seasons
nfldata = load_pbp(2019:2024)

# calculate regular season PPG by game
ppgdata = nfldata %>%
  filter(season_type == "REG",
         home_team == "IND" | away_team == "IND") %>%
  group_by(game_id,
           season,
           week) %>%
  summarize(ind_score = max(ifelse(home_team == "IND", total_home_score, total_away_score))) %>%
  print(n = Inf)

# calc number of plays per game
playsdata = nfldata %>%
  filter(pass == 1 | rush == 1,
         qb_spike == 0,
         qb_kneel == 0,
         posteam == "IND") %>%
  group_by(game_id,
           season,
           week) %>%
  summarize(plays = n()) %>%
  print(n = Inf)

# join tibbles
points_plays = ppgdata %>%
  left_join(playsdata, by = c("game_id", "season", "week")) %>%
  print(n = Inf)

# calc points per play and per game by season
ind_stats_24 = points_plays %>%
  group_by(season) %>%
  summarize(ppg = mean(ind_score),
            ppp = sum(ind_score)/sum(plays)) %>%
  print(n = Inf)

# plot ppg by season
ppg_plot = ggplot(data = ind_stats_24,
                  aes(x = season, y = ppp)) +
  geom_hline(yintercept = mean(ppg_season$ppp),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.5) +
  geom_point(color = "#002C5F",
             size = 3) +
  geom_line(color = "#002C5F") +
  labs(title = "Colts Points Per Play by Season",
       subtitle = "2019-2024 Regular Seasons",
       x = "Season",
       y = "Points/Play",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 22),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold.italic",
                                     size = 14),
        plot.caption = element_text(size = 13),
        axis.title.y = element_text(face = "bold",
                                    size = 16),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(face = "bold",
                                    size = 16),
        axis.text.x = element_text(size = 14))

# view plot
ppg_plot

# save plot to local files
ggsave("X post 64.1 - colts_ppp.png",
       width = 10.5, height = 7,
       dpi = "retina")


# Plot 2 - TD Breakdown ---------------------------------------------------

# offensive touchdowns
colts_td_a = nfldata_24 %>%
  mutate(play_location = ifelse(play_type == "run", run_location, pass_location)) %>%
  filter(season == 2024,
         season_type == "REG",
         posteam == "IND",
         touchdown == 1) %>%
  select(game_id,
         posteam,
         posteam_score,
         defteam_score,
         yardline_100,
         play_type,
         pass_location,
         run_location,
         play_location) %>%
  filter(!is.na(play_location)) %>%   #excludes turnovers from IND offense that resulted in defensive touchdown
  print(n = Inf)

# plot TDs
td_plot_a = colts_td_a %>%
  ggplot(aes(x = play_location,
             y = yardline_100,
             shape = play_type,
             color = play_type)) +
  geom_beeswarm(size = 3) +
  scale_color_manual(values = c("orange", "purple")) +
  scale_y_reverse() +
  labs(title = "Colts Offensive Touchdowns",
       subtitle = "2024 Regular Season",
       x = "Play Location",
       y = "Yard Line",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme(panel.background = element_rect(fill = "#228B2250", 0.25),
      plot.title = element_text(hjust = 0.5,
                                face = "bold",
                                size = 18),
      plot.subtitle = element_text(hjust = 0.5,
                                   face = "bold.italic",
                                   size = 12),
      plot.caption = element_text(size = 13),
      axis.title.y = element_text(face = "bold",
                                  size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(face = "bold",
                                  size = 14),
      axis.text.x = element_text(size = 14))

# view plot
td_plot_a

# save plot to local files
ggsave("X post 64.2 - colts_td_dist.png",
       width = 10.5, height = 7,
       dpi = "retina")


# Plot 3 - Success Rate Comp ----------------------------------------------

# calc rushing success rate
rush_success = nfldata %>%
  filter(season == 2024,
         season_type == "REG",
         rush == 1 | qb_scramble == 1,
         sack == 0,
         qb_kneel == 0,
         two_point_attempt == 0,
         !is.na(success)) %>%
  group_by(posteam) %>%
  summarize(rushes = n(),
            rush_success = sum(success)/rushes) %>%
  arrange(-rush_success) %>%
  print(n = Inf)

# calc passing success rate
pass_success = nfldata %>%
  filter(season == 2024,
         season_type == "REG",
         !is.na(passer_player_id),
         qb_spike == 0,
         two_point_attempt == 0,
         !is.na(success)) %>%
  group_by(posteam) %>%
  summarize(passes = n(),
            pass_success = sum(success)/passes) %>%
  arrange(-pass_success) %>%
  print(n = Inf)

# join tibbles
success_rate_data = rush_success %>%
  left_join(pass_success, by = "posteam") %>%
  print(n = Inf)

# plot rush success vs. pass success rates
success_rate_plot = success_rate_data %>%
  mutate(color3 = ifelse(posteam == "IND", NA, "b/w")) %>%
  mutate(width3 = ifelse(posteam == "IND", 0.065, 0.050)) %>%
  ggplot(aes(x = rush_success,
             y = pass_success)) +
  geom_hline(yintercept = mean(success_rate_data$pass_success),
             linetype = "dashed",
             color = "red",
             alpha = 0.65) + 
  geom_vline(xintercept = mean(success_rate_data$rush_success),
             linetype = "dashed",
             color = "red",
             alpha = 0.65) +
  geom_nfl_logos(aes(team_abbr = posteam,
                     color = color3,
                     width = width3),
                     alpha = 0.800) +
  scale_color_identity() +
  labs(title = "Rushing vs. Passing Success Rate",
       subtitle = "2024 Regular Season",
       x = "Rushing Success Rate",
       y = "Passing Success Rate",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold.italic",
                                     size = 14),
        plot.caption = element_text(size = 13),
        axis.title.y = element_text(face = "bold",
                                    size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(face = "bold",
                                    size = 14),
        axis.text.x = element_text(size = 14))

# view plot
success_rate_plot

# save plot to local files
ggsave("X post 64.3 - colts_success.png",
       width = 10.5, height = 7,
       dpi = "retina")


# Table 1 - QB Comparison -------------------------------------------------

# calc games played in 2024
qb_gp = nfldata %>%
  filter(season == 2024,
         season_type == "REG",
         id == "00-0035710" | id == "00-0039164") %>%
  group_by(id,
           name) %>%
  summarize(gp = sum(n_distinct(game_id))) %>%
  print(n = Inf)

# calc passing stats
qb_passing = nfldata %>%
  filter(season == 2024,
         season_type == "REG",
         passer_player_id == "00-0035710" | passer_player_id == "00-0039164",
         qb_spike == 0,
         !is.na(cpoe)) %>%
  group_by(id,
           name,
           posteam) %>%
  summarize(pass_att = n(),
            yd_att = sum(yards_gained)/pass_att,
            cpoe = mean(cpoe),
            air_yds = mean(air_yards)) %>%
  print(n = Inf)

# combine tibbles
qb_comp_data = qb_gp %>%
  left_join(qb_passing, by = c("id", "name")) %>%
  print(n = Inf)

# calc overall stats
qb_stats = nfldata %>%
  filter(season == 2024,
         season_type == "REG",
         id == "00-0035710" | id == "00-0039164",
         qb_kneel == 0,
         qb_spike == 0,
         !is.na(epa),
         !is.na(success)) %>%
  group_by(id,
           name,
           posteam) %>%
  summarize(plays = n(),
            epa_play = sum(epa)/plays,
            succ_rate = sum(success)/plays) %>%
  print(n = Inf)

# create final tibble, joining new data with previously joined data
# reorder columns for table display
qb_comp_data_a = qb_comp_data %>%
  left_join(qb_stats, by = c("id", "name", "posteam")) %>%
  relocate(posteam, .before = gp) %>%
  print(n = Inf)

# drop player name column
# ungroup to include `id` as variable
qb_comp_data_b = qb_comp_data_a %>%
  select(id,
         posteam,
         gp,
         yd_att,
         cpoe,
         air_yds,
         air_yds,
         epa_play,
         succ_rate) %>%
  ungroup()

# create table
qb_comp_tbl = gt(qb_comp_data_b) %>%
  gt_theme_espn() %>%
  cols_label(id = "",
             posteam = "Team",
             yd_att = "Yds/Att",
             cpoe = "CPOE",
             air_yds = "Air Yd/Att",
             epa_play = "EPA/Play",
             succ_rate = "Success Rate") %>%
  cols_align(align = "center") %>%
  fmt_number(columns = c("yd_att",
                         "cpoe",
                         "air_yds",
                         "epa_play",
                         "succ_rate"),
             decimals = 2) %>%
  gt_nfl_headshots("id",
                   height = 50) %>%
  tab_header(title = "2025 Colts QB Comparison",
             subtitle = "'24 Regular Season Stats") %>%
  opt_align_table_header(align = "center") %>%
  tab_footnote(footnote = md("By Nick Gasperi | @tbanalysis | data @nflfastR")) %>%
  tab_options(footnotes.font.size = 11)

# view table
qb_comp_tbl

# save table
qb_comp_tbl %>%
  gtsave("X post 64.4 - qb_comp_tbl.png",
         expand = 9)
