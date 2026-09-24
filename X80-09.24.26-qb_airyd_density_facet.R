# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(ggthemes)
library(scales)

# load 2025 pbp data
data25 = load_pbp(2025)

# set top 6 2025 total pass yd leaders in reg season
# will be used to define which qbs to use in density plot
passyd_leaders_25 = data25 %>%
  filter(season_type == "REG",
         play_type == "pass",
         !is.na(passer_player_id),
         !is.na(air_yards),
         !is.na(passing_yards),
         aborted_play == 0,
         qb_spike == 0,
         sack == 0,
         qb_scramble == 0) %>%
  select(passer_player_id,
         passer_player_name,
         posteam,
         passing_yards,
         air_yards) %>%
  group_by(passer_player_id,
           passer_player_name) %>%
  summarize(tot_passyd = sum(passing_yards),
            tot_airyd = sum(air_yards),
            .groups = "drop") %>%
  arrange(-tot_passyd) %>%
  select(passer_player_id) %>%
  slice_head(n = 6)

# get plot data
pass_plot_data_25 = data25 %>%
  filter(season_type == "REG",
         play_type == "pass",
         !is.na(air_yards),
         !is.na(passing_yards),
         aborted_play == 0,
         qb_spike == 0,
         sack == 0,
         qb_scramble == 0,
         passer_player_id %in% passyd_leaders_25$passer_player_id) %>%
  select(passer_player_id,
         passer_player_name,
         posteam,
         air_yards)

# set unique list of qbs and their posteam values for wordmarks in the plot
wordmark_list = pass_plot_data_25 %>%
  distinct(passer_player_name,
           posteam)

# plot
passing25plot = ggplot(data = pass_plot_data_25,
                       aes(x = air_yards,
                           fill = posteam)) +
  geom_density(alpha = 0.6) +
  scale_fill_nfl() +
  facet_wrap(~ passer_player_name) +
  scale_x_continuous(breaks = seq(0, 45,
                                  by = 15)) +
  geom_nfl_wordmarks(data = wordmark_list,
                     aes(x = 35.000,
                         y = 0.078,
                         team_abbr = posteam),
                     width = 0.45) +
  labs(title = "Density Plot of Air Yards on Completed Passes",
       subtitle = "'25 NFL Reg. Season | Top 6 QBs in Total Pass Yards",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Air Yards",
       y = "Density") +
  theme_hc() +
  theme(strip.text = element_text(face = "bold",
                                  size = 15),
        strip.background = element_rect(fill = "wheat",
                                        color = NA),
        plot.background = element_rect(fill = "#F2F4F5",
                                       color = NA),
        plot.title = element_text(size = 18,
                                  face = "bold"),
        plot.subtitle = element_text(size = 14,
                                     face = "bold"),
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 15,
                                  face = "bold"),
        axis.text = element_text(size = 12))
  
# view plot
passing25plot

# save plot to local files
ggsave("X post 80 - qb_airyd_density_facet.png",
       width = 10.5, height = 8,
       dpi = "retina")
