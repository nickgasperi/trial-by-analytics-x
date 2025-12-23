# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggrepel)

# load data
nfldata25 = load_pbp(2025)

# this post requires two tibbles - one for the stats and another to pull posteam for each qb_passer_id
# must match posteam to id separately bc one id can have multiple unique posteam values in the dataset
# the two tibbles will be joined to create a third, final tibble for the plot

# define qbs and associated (most recent) posteam
# arrange by game date desc to bring the most recent games to the top of the selection
# then use first(posteam), which grabs the qb's posteam from the most recent game by selecting from the top of the list 
qb_teams = nfldata25 %>%
  filter(!is.na(passer_player_id),
         week < 17,
         qb_dropback == 1) %>%
  arrange(desc(game_date)) %>%
  group_by(passer_player_id) %>%
  summarize(dropbacks = n(),
            posteam = first(posteam)) %>%
  filter(dropbacks >= 10) %>%
  select(passer_player_id,
         posteam) %>%
  print(n = Inf)
  
# new tibble
# Q4/OT + score tied or less thru week 16
# at least 20 dropbacks
qbdata1 = nfldata25 %>%
  filter(!is.na(passer_player_id),
         week < 17,
         qtr %in% c(4, 5),
         posteam_score <= defteam_score,
         qb_dropback == 1,
         qb_kneel == 0,
         qb_spike == 0) %>%
  group_by(passer_player_id,
           passer_player_name) %>%
  summarize(dropbacks = n(),
            epa_per = sum(epa)/sum(dropbacks),
            .groups = "drop") %>%
  filter(dropbacks >= 45) %>%
  arrange(-dropbacks) %>%
  print(n = Inf)

# join tibbles
qb_plot_data_1 = qbdata1 %>%
  left_join(qb_teams,
            by = "passer_player_id")

# view final tibble
qb_plot_data_1 %>%
  print(n = Inf)

# plot data
qb_plot_1 = ggplot(data = qb_plot_data_1,
                   aes(x = dropbacks,
                       y = epa_per)) +
  geom_hline(yintercept = mean(qb_plot_data_1$epa_per),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.60) +
  geom_vline(xintercept = mean(qb_plot_data_1$dropbacks),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.60) +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.04,
                 alpha = 0.70) +
  geom_text_repel(box.padding = 0.50,
                  aes(label = passer_player_name,
                      color = posteam,
                      segment.alpha = 0.0)) +
  scale_color_nfl(type = "primary") +
  labs(title = "QB EPA/Dropback - 4th Qtr & OT While Trailing or Tied",
       subtitle = "2025 NFL Wk 1-16 | min. 45 dropbacks",
       x = "# of Dropbacks",
       y = "EPA/Dropback",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold",
                                  size = 16),
        plot.subtitle = element_text(face = "bold",
                                     size = 14),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold",
                                  size = 13),
        axis.text = element_text(size = 12))

# view plot
qb_plot_1

# save plot to local files
ggsave("X post 74 - epa_late_dropbacks.png",
       width = 10.5, height = 7,
       dpi = "retina")



  
