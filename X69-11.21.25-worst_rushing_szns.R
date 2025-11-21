library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load 2020s nfl data
nfldata20 = load_pbp(2020:2025)
# regular season worst rushing totals by player and season
# cannot use play_type since it won't count kneels/scrambles/others, so use !is.na(rusher_player_id)
# include players with at least 20 attempts and yards per rush less than 1.5
rushing_data = nfldata20 %>%
  filter(!is.na(rusher_player_id),
         season_type == "REG") %>%
  group_by(season,
           posteam,
           rusher_player_id,
           rusher_player_name) %>%
  summarize(att = n(),
            yds = sum(yards_gained),
            ypc = yds/att) %>%
  filter(att >= 20,
         ypc < 1.50) %>%
  arrange(ypc) %>%
  print(n = Inf)

# add extra column for label
rushing_data$label_id = paste(rushing_data$rusher_player_name,
                              rushing_data$season,
                              sep = ", ")

# plot data
rushing_plot = ggplot(data = rushing_data,
                      aes(x = yds,
                          y = ypc)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "grey20") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "grey20") +
  geom_point(aes(size = rushing_data$att,
                 color = posteam)) +
  geom_text_repel(box.padding = 0.3,
                  aes(label = label_id,
                      color = posteam)) +
  scale_color_nfl() +
  labs(title = "Worst Rushing Seasons of the 2020s",
       subtitle = "2020-2025 Reg. Seasons (thru '25 Wk 11) | size = attempts | min. 20 attempts",
       x = "Total Rush Yards",
       y = "Yards Per Carry",
       caption = "") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold",
                                  size = 18),
        plot.subtitle = element_text(face = "bold",
                                     size = 15),
        plot.caption = element_text(size = 12),
        axis.title = element_text(face = "bold",
                                  size = 14),
        axis.text = element_text(size = 13))

# view plot
rushing_plot

# save plot to local files
ggsave("X post 69 - worst_rushing_szns.png",
       width = 10.5, height = 7,
       dpi = "retina")


