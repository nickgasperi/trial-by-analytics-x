# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2026 week 1 pbp data
data26_1 = load_pbp(2026)

# filter to qb data including attempts, cpoe, and adot
qb_data_26_1 = data26_1 %>%
  filter(!is.na(passer_player_id),
         !is.na(air_yards),
         qb_spike == 0) %>%
  group_by(passer_player_id,
           passer_player_name,
           posteam) %>%
  summarize(att = n(),
            cpoe = mean(cpoe,
                        na.rm = TRUE),
            adot = mean(air_yards,
                        na.rm = TRUE)) %>%
  filter (att > 5) %>%
  arrange(-cpoe) %>%
  print(n = Inf)

# plot adot vs. cpoe
qb_data_26_plot = ggplot(data = qb_data_26_1,
                         aes(x = adot, y = cpoe)) +
  geom_hline(yintercept = mean(qb_data_26_1$cpoe),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.6) +
  geom_vline(xintercept = mean(qb_data_26_1$adot),
             linetype = "dashed",
             color = "grey20",
             alpha = 0.6) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "grey") +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.04,
                 alpha = 0.80) +
  geom_text_repel(segment.color = NA,
                  aes(label = passer_player_name,
                      color = posteam)) +
  scale_color_nfl() +
  labs(title = "ADOT vs. CPOE",
       subtitle = "2026 NFL Week 1",
       x = "ADOT",
       y = "CPOE",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold",
                                  size = 17),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold",
                                  size = 14),
        axis.text = element_text(size = 13))

# view plot
qb_data_26_plot

# save plot to local files
ggsave("X post 76 - adot_cpoe_wk1.png",
       width = 10.5, height = 7,
       dpi = "retina")
