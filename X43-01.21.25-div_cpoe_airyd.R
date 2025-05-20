# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
nfldata = load_pbp(2024)

# filter data
divround = nfldata %>%
  filter(week == 20,
         qb_spike == 0,
         !is.na(air_yards),
         !is.na(cpoe)) %>%
  group_by(passer_player_id,
           posteam,
           passer_player_name) %>%
  summarize(att = n(),
            cpoe = mean(cpoe),
            airyd = mean(air_yards)) %>%
  filter(att >= 20) %>%
  print(n = Inf)

# plot data
divqbplot1 = ggplot(data = divround,
                    aes(x = airyd, y = cpoe)) +
  geom_vline(xintercept = mean(divround$airyd),
             linetype = "dashed",
             color = "red") +
  geom_hline(yintercept = mean(divround$cpoe),
             linetype = "dashed",
             color = "red") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey") +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.075,
                 alpha = 0.8) +
  geom_text_repel(box.padding = 0.8,
                  aes(label = divround$passer_player_name,
                      color = posteam,
                      size = 3,
                      fontface = "bold.italic")) +
  scale_color_nfl(type = "primary") +
  scale_y_continuous(n.breaks = 6) +
  labs(title = "Air Yards vs. CPOE Per Attempt",
       subtitle = "2024 NFL Divisional Round",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Air Yards",
       y = "CPOE") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 18),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 16),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text = element_text(size = 15))

# view plot
divqbplot1

# save plot to local files
ggsave("X post 43 - div_cpoe_airyd.png",
       width = 10.5, height = 7,
       dpi = "retina")
