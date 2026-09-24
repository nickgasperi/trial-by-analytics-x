# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(ggbeeswarm)

# load 2025 pbp data
data25_1 = load_pbp(2025)

# filter to Kenneth Walker rushing plays
rush_area_25 = data25_1 %>%
  filter(play_type == "run",
         rusher_player_id == "00-0038134",
         !is.na(yards_gained),
         aborted_play == 0,
         sack == 0) %>%
  select(rusher_player_id,
         rusher_player_name,
         posteam,
         run_location,
         run_gap,
         yards_gained,
         touchdown,
         first_down)

# add run_area variable by combining run location and run gap
rush_area_25$run_area = paste(rush_area_25$run_location,
                              rush_area_25$run_gap,
                              sep = "-")

# change datatype to factor, manually resort, and rename before plotting
rush_area_25$run_area = factor(rush_area_25$run_area,
                               levels = c("left-end", "left-tackle", "left-guard",
                                          "middle-NA",
                                          "right-guard", "right-tackle", "right-end"),
                               labels = c("Left End", "Left Tackle", "Left Guard",
                                          "Center",
                                          "Right Guard", "Right Tackle", "Right End"))

# plot rushing distribution
rush_area_plot_test = rush_area_25 %>%
  mutate(color5 = ifelse(touchdown == 1,
                         "green3",
                         ifelse(first_down == 1,
                                "orange",
                                "grey30"))) %>%
  ggplot(aes(x = run_area,
             y = yards_gained)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "blue") +
  geom_beeswarm(aes(color = color5),
                size = 2) +
  scale_color_identity(name = NULL,
                       breaks = c("green3",
                                  "orange"),
                       labels = c("Touchdown",
                                  "First Down"),
                       guide = "legend") +
  guides(color = guide_legend(override.aes = list(size = 3,
                                                  nrow = 1))) +
  labs(title = "Kenneth Walker Rushing Distribution",
       subtitle = "2025 NFL Regular Season & Playoffs",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "",
       y = "Yards Gained",
       tag = "SEA") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white",
                                       color = NA),
        plot.title = element_text(face = "bold",
                                  size = 19),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 12),
        axis.title = element_text(face = "bold",
                                  size = 16),
        axis.text = element_text(size = 14),
        legend.position = c(0.0, 0.95),
        legend.justification = c(0.00, 0.00),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(color = "grey35",
                                   size = 12),
        plot.tag = element_nfl_wordmark(size = 5.5),
        plot.tag.position = c(0.89, 0.96))

# view plot
rush_area_plot_test

# save plot to local files
ggsave("X post 78 - kw_rush_distribution.png",
       width = 10.5, height = 7,
       dpi = "retina")
