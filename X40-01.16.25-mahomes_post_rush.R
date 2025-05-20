# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load data from 2018-2023 NFL seasons
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

# create plot 1
pmplot1 = kcqb1 %>%
  mutate(colorkc1 = ifelse(game_id %in% divgame1, NA, "b/w")) %>%
  ggplot(aes(x = game_id,
             y = rushes,
             group = 1)) +
  scale_x_discrete(label = game_labels) +
  scale_y_continuous(n.breaks = 8) +
  geom_abline(slope = 0,
              intercept = seq(c(1, 2, 3, 4, 5, 6, 7, 8, 9)),
              alpha = 0.45,
              color = "grey") +
  geom_line(color = "red", alpha = 0.8) + 
  geom_hline(yintercept = 4.5,
             linetype = "dashed",
             color = "blue") +
  geom_nfl_logos(aes(team_abbr = posteam,
                     color = colorkc1),
                 width = 0.06) +
  scale_color_identity() +
  annotate("text",
           label = "Mahomes O/U 4.5 Att.",
           x = 17, y = 4.65,
           fontface = "bold",
           color = "blue",
           size = 4.5) +
  labs(title = "Mahomes Rush Att. by Playoff Game",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Game",
       y = "Rush Attempts") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 18),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold.italic",
                                  size = 14),
        axis.text = element_text(size = 15))

# view plot 1
pmplot1

# save plot 1 to local files
ggsave("X post 40.1 - mahomes_rush.png",
       width = 10.5, height = 7,
       dpi = "retina")

# create plot 2
pmplot2 = ggplot(data = kcqb1,
                 aes(x = rushes)) +
  geom_histogram(bins = 8,
                 binwidth = 1.0,
                 fill = "gold",
                 color = "red",
                 linewidth = 0.75,
                 alpha = 0.8) +
  geom_vline(xintercept = 4.5,
             linetype = "dashed",
             color = "blue",
             linewidth = 1.0) +
  scale_color_identity() +
  scale_x_continuous(n.breaks = 8) +
  labs(title = "Distribution of Mahomes Rush Att. by Playoff Game",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Rush Attempts",
       y = "Frequency",
       tag = "00-0033873") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.tag = element_nfl_headshot(size = 4.25,
                                        hjust = 1,
                                        vjust = 1),
        plot.tag.position = c(1,1),
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(face = "bold",
                                  size = 18),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold.italic",
                                  size = 14),
        axis.text = element_text(size = 14))

# view plot 2
pmplot2

# save plot 2 to local files
ggsave("X post 40.2 - mahomes_rush.png",
       width = 10.5, height = 7,
       dpi = "retina")
