# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load NFL data from '18-'23 seasons
pmdata1 = load_pbp(2018:2023)

# define divisional games for KC
divgame1 = c("2018_19_IND_KC", "2019_19_HOU_KC", "2020_19_CLE_KC",
             "2021_20_BUF_KC", "2022_20_JAX_KC", "2023_20_KC_BUF")

# create manual label for future plot x axis
game_labels = seq(1:18)

# filter data to KC playoff games while Mahomes has been starter
# filter to only Mahomes rushes
kcqb1 = pmdata1 %>%
  filter(season_type == "POST",
         rusher_player_name == "P.Mahomes") %>%
  group_by(game_id,
           rusher_player_id,
           rusher_player_name,
           posteam) %>%
  summarize(rushes = n(),
            rushyd = sum(yards_gained),
            ydper = sum(rushyd)/sum(rushes)) %>%
  print(n = Inf)

# create plot
pmplot3 = kcqb1 %>%
  mutate(colorkc1 = ifelse(game_id %in% divgame1, NA, "b/w")) %>%
  ggplot(aes(x = game_id,
             y = rushyd,
             group = 1)) +
  scale_x_discrete(label = game_labels) +
  scale_y_continuous(n.breaks = 8) +
  geom_line(color = "red",
            alpha = 0.8) + 
  geom_hline(yintercept = 24.5,
             linetype = "dashed",
             color = "blue") +
  geom_hline(yintercept = 29.1,
             linetype = "dashed",
             color = "darkgreen") +
  geom_nfl_logos(aes(team_abbr = posteam,
                     color = colorkc1),
                 width = 0.06) +
  scale_color_identity() +
  annotate("text",
           label = "O/U 24.5 Yd",
           x = 16, y = 26.2,
           fontface = "bold",
           color = "blue",
           size = 4.5) +
  annotate("text",
           label = "Avg. = 29.1 Yd",
           x = 12.5, y = 30.8,
           fontface = "bold",
           color = "darkgreen",
           size = 4.5) + 
  labs(title = "Mahomes Rush Yards by Playoff Game",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Game",
       y = "Rush Yards") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(face = "bold",
                                  size = 18),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold.italic",
                                  size = 14),
        axis.text = element_text(size = 15))

# view plot
pmplot3

# save plot to local files
ggsave("X post 41 - mahomes_rush_yd.png",
       width = 10.5, height = 7,
       dpi = "retina")
