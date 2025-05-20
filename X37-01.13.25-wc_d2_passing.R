# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load 2024 NFL data
nfldata = load_pbp(2024)

# filter data to see wild card day 2 passing
day2qbs = nfldata %>%
  mutate(passing_yards = ifelse(is.na(passing_yards), 0, passing_yards)) %>%
  filter(week == 19,
         posteam == "DEN" | posteam == "BUF" | posteam == "WAS" |
         posteam == "GB" | posteam == "PHI" | posteam == "TB",
         play_type == "pass", qb_spike == 0,
         !is.na(epa),
         !is.na(air_yards),
         !is.na(passer_player_id)) %>%
  group_by(passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(att = n(),
            passyd = sum(passing_yards),
            epaper = sum(epa)/sum(att)) %>%
  arrange(-epaper) %>%
  filter(att >= 10) %>%
  print(n = Inf)

# plot data
# geom_segment() replaces geom_col() - use geom_nfl_logos() instead of geom_point()
day2qbplot = ggplot(data = day2qbs,
                    aes(x = epaper,
                        xend = 0,
                        y = reorder(passer_player_id, epaper),
                        yend = passer_player_id)) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "darkgrey") +
  scale_x_continuous(n.breaks = 8) +
  geom_segment(linewidth = 3.25,
               aes(color = posteam)) +
  scale_color_nfl(type = "primary") +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.07) +
  theme_minimal() +
  labs(title = "EPA Per Pass Attempt",
       subtitle = "2024 NFL Wild Card Weekend - Day 2",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "EPA/Attempt") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 18),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 16),
        plot.caption = element_text(size = 11),
        axis.title.y = element_blank(),
        axis.text.y  = element_nfl_headshot(size = 2.6),
        axis.title.x = element_text(face = "bold",
                                    size = 14),
        axis.text.x = element_text(size = 14))

# view plot
day2qbplot

# save plot
ggsave("X post 37 - wc_d2_pass.png",
       width = 10.5, height = 6,
       dpi = "retina")
