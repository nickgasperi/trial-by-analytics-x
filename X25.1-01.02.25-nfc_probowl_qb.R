# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
nfldata = load_pbp(2024)

# set qb and team groups for later viz
qbnames1 = c("J.Goff", "B.Mayfield", "S.Darnold", "J.Daniels")
teamnames1 = c("DET", "TB", "MIN", "WAS")

# filter data
nflqbs = nfldata %>%
  filter(play_type == "pass",
         !is.na(passer_player_name),
         !is.na(air_yards),
         !is.na(yards_gained),
         !is.na(epa),
         !is.na(success),
         !is.na(touchdown),
         qb_spike == 0) %>%
  mutate(cpoe = ifelse(is.na(cpoe), 0, cpoe)) %>%
  group_by(passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(att = n(),
            epa = sum(epa)/sum(att),
            succ = sum(success)/sum(att),
            cpoe = mean(cpoe),
            yds = sum(yards_gained),
            airyds = sum(air_yards)/sum(att)) %>%
  filter(att >= 250) %>%
  arrange(-att) %>%
  print(n = Inf)

# plot data
nfcqbplot = nflqbs %>%
  mutate(color2 = ifelse(posteam %in% teamnames1, NA, "b/w")) %>%
  mutate(width2 = ifelse(posteam %in% teamnames1, 0.08, 0.05)) %>%
  mutate(alpha2 = ifelse(posteam %in% teamnames1, 1.5, 1)) %>%
  mutate(name2 = ifelse(passer_player_name %in% qbnames1, passer_player_name, "")) %>%
  ggplot(aes(x = airyds, y = cpoe)) +
  geom_hline(yintercept = mean(nflqbs$cpoe),
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = mean(nflqbs$airyds),
             linetype = "dashed",
             color = "red") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey") +
  geom_nfl_logos(aes(team_abbr = posteam,
                     color = color2,
                     width = width2,
                     alpha = alpha2)) +
  geom_text_repel(box.padding = 1.5,
                  aes(label = name2,
                      fontface = "bold",
                      size = 2)) +
  scale_color_identity() +
  labs(title = "Air Yards Per Attempt vs. CPOE",
       subtitle = "2024 NFL Wk 1-17 | min. 250 att.",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Air Yards",
       y = "CPOE") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 19),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 17),
        plot.caption = element_text(size = 10),
        axis.title = element_text(face = "bold.italic",
                                  size = 13),
        axis.text = element_text(size = 13))

# view plot
nfcqbplot

# save plot to local files
ggsave("X post 25 - probowl_qbs.png",
       width = 10.5, height = 7,
       dpi = "retina")
