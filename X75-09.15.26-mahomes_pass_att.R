# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(png)
library(grid)

# load chiefs logo image
chiefs_logo = "C:/Users/Nick Gasperi/Downloads/old_chiefs_logo.png"

# upload image
chiefs_logo_img = readPNG(chiefs_logo) %>%
  rasterGrob(interpolate = TRUE)

# load 2024-2026 week 1 pbp data
data24_26 = load_pbp(2024:2026)

# filter to Mahomes reg. season pass att. by game from '24-'26
mahomes_pass_att = data24_26 %>%
  filter(season_type == "REG",
         passer_player_name == 'P.Mahomes',
         !is.na(air_yards)) %>%
  group_by(game_id) %>%
  summarize(pass_att = n()) %>%
  arrange(pass_att) %>%
  print(n = Inf)

# select games with lowest pass att. to highlight in plot
mahomes_pass_att = mahomes_pass_att %>%
  mutate(highlight = case_when(game_id == "2024_02_CIN_KC" ~ "red",
                               game_id %in% c("2024_07_KC_SF",
                                              "2026_01_DEN_KC") ~ "orange3",
                               TRUE ~ "grey75"),
         col_alpha = case_when(game_id %in% c("2024_02_CIN_KC",
                                          "2024_07_KC_SF",
                                          "2026_01_DEN_KC") ~ 1.0,
                           TRUE ~ 0.6))

# plot pass att. by game col chart
mahomes_pass_att_plot = ggplot(data = mahomes_pass_att,
                               aes(x = game_id,
                                   y = pass_att,
                                   fill = highlight)) +
  geom_col(alpha = mahomes_pass_att$col_alpha) +
  scale_fill_identity() +
  geom_hline(yintercept = c(25, 27),
             linetype = "dashed",
             linewidth = 1.25,
             color = c("red", "orange3")) +
  annotate("text",
           x = 13, y = 28.5,
           label = "27 att.",
           color = "orange3",
           size = 7.0,
           fontface = "bold") +
  annotate("text",
           x = c(9, 28), y = c(7, 10),
           label = c("2024 Wk 7", "2026 Wk 1"),
           color = "grey25",
           size = 7.0,
           fontface = "italic") +
  geom_segment(aes(x = 8.5,
                   y = 9.0,
                   xend = 6.1,
                   yend = 14.0),
               arrow = arrow(),
               color = "grey25",
               linewidth = 0.8) +
  geom_segment(aes(x = 28.3,
                   y = 11.0,
                   xend = 30.9,
                   yend = 16.0),
               arrow = arrow(),
               color = "grey25",
               linewidth = 0.8) +
  coord_cartesian(clip = "off") +
  annotation_custom(chiefs_logo_img,
                    x = 25, y = 45,
                    xmax = 35, ymax = 53) +
  labs(title = "Patrick Mahomes Pass Attempts by Game",
       subtitle = "2024-2026 NFL Regular Seasons thru 2026 Wk 1",
       x = "Game (chronological)",
       y = "Pass Att.",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold",
                                  size = 17),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold",
                                  size = 14),
        axis.text = element_text(size = 13),
        axis.text.x = element_blank())

# view plot
mahomes_pass_att_plot

# save plot to local files
ggsave("X post 75 - mahomes_pass_att.png",
       width = 10.5, height = 7,
       dpi = "retina")
