# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load 2024 NFL data
nfldata = load_pbp(2024)

# filter data
day2rec = nfldata %>%
  mutate(receiving_yards = ifelse(is.na(receiving_yards), 0, receiving_yards)) %>%
  filter(week == 19,
         posteam == "DEN" | posteam == "BUF" | posteam == "WAS" |
         posteam == "GB" | posteam == "PHI" | posteam == "TB",
         play_type == "pass",
         !is.na(epa),
         !is.na(air_yards),
         !is.na(receiver_player_id)) %>%
  group_by(receiver_player_id,
           receiver_player_name,
           posteam) %>%
  summarize(targets = n(),
            avgepa = sum(epa)/sum(targets),
            totepa = sum(epa),
            recyd = sum(receiving_yards)) %>%
  arrange(-avgepa) %>%
  filter(targets >= 5) %>%
  print(n = Inf)

# plot data
# geom_segment() replaces geom_col() - use geom_nfl_logos() instead of geom_point()
day2recplot = ggplot(data = day2rec,
                     aes(x = avgepa,
                         xend = 0,
                         y = reorder(receiver_player_id, avgepa),
                         yend = receiver_player_id)) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "darkgrey") +
  scale_x_continuous(limits = c(-0.5, 2)) +
  geom_segment(linewidth = 3,
               aes(color = posteam)) +
  scale_color_nfl(type = "primary") +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.06) +
  theme_minimal() +
  labs(title = "EPA Per Target",
       subtitle = "2024 NFL Wild Card Weekend - Day 2",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "EPA/Target") +
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
        axis.text.y  = element_nfl_headshot(size = 2.075),
        axis.title.x = element_text(face = "bold",
                                    size = 14),
        axis.text.x = element_text(size = 14))

# view plot
day2recplot

# save plot to local files
ggsave("X post 38 - wc_d2_rec.png",
       width = 10.5, height = 6.5,
       dpi = "retina")
