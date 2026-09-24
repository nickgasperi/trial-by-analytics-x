# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load data
data26 = load_pbp(2026)

# get total air yards and yards after catch by team
pass26 = data26 %>%
  filter(play_type == "pass",
         qb_spike == 0,
         sack == 0,
         qb_scramble == 0,
         !is.na(air_yards),
         !is.na(yards_after_catch)) %>%
  select(posteam,
         air_yards,
         yards_after_catch) %>%
  group_by(posteam) %>%
  summarize(tot_air_yards = sum(air_yards),
            tot_yac = sum(yards_after_catch)) %>%
  print(n = Inf)

# plot data
pass26plot1 = ggplot(data = pass26,
                     aes(x = tot_yac,
                         y = tot_air_yards)) +
  geom_hline(yintercept = mean(pass26$tot_air_yards),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.6) +
  geom_vline(xintercept = mean(pass26$tot_yac),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.6) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey") +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.06,
                 alpha = 0.80) +
  scale_color_nfl() +
  labs(title = "Total Yards After Catch vs. Air Yards",
       subtitle = "2026 NFL Wk 1-2",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Yards After Catch",
       y = "Air Yards") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(plot.background = element_rect("#F2F4F5",
                                       color = NA),
        plot.margin = margin(t = 10,
                             r = 20,
                             b = 10,l = 10),
        plot.title = element_text(face = "bold",
                                  size = 19),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 12),
        axis.title = element_text(face = "bold",
                                  size = 16),
        axis.text = element_text(size = 14))

# view plot
pass26plot1

# save plot to local files
ggsave("X post 79 - yac_airyds.png",
       width = 10.5, height = 7,
       dpi = "retina")
