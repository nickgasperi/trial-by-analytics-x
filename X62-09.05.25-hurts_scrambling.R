# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(png)
library(grid)

# load eagles logo image
image = "C:/Users/Nick Gasperi/Downloads/Philadelphia-Eagles-Logo-1987-1995.png"

# download image to R
eagles_logo = readPNG(image) %>%
  rasterGrob(interpolate = TRUE)

# load 2020-2025 NFL data
nfldata = load_pbp(2020:2025)

# new tibble with Hurts stats per dropback
hurts1 = nfldata %>%
  filter(!is.na(game_id),
         !is.na(epa),
         !is.na(qb_dropback),
         !is.na(qb_scramble),
         season_type == "REG",
         name == "J.Hurts",
         posteam == "PHI",
         qb_kneel == 0,
         qb_spike == 0) %>%
  group_by(season,
           week,
           game_id,
           posteam) %>%
  summarize(dropbacks = n(),
            scrambles = sum(qb_scramble),
            scram_rate = scrambles/dropbacks,
            epa_per_d = sum(epa)/dropbacks) %>%
  filter(dropbacks >= 20) %>%
  arrange(season,
          week) %>%
  print(n = Inf)

# plot scramble rate vs. epa/dropback by game
hurtsplot1 = ggplot(data = hurts1,
                    aes(x = scram_rate, y = epa_per_d)) +
  geom_hline(yintercept = mean(hurts1$epa_per_d),
             linetype = "dashed",
             color = "cornsilk") +
  geom_vline(xintercept = mean(hurts1$scram_rate),
             linetype = "dashed",
             color = "cornsilk") +
  geom_point(color = "#00C957") +
  coord_cartesian(clip = "off") +
  annotation_custom(eagles_logo,
                    x = 0.219, y = 0.76,
                    xmax = 0.278, ymax = 1.02) +
  geom_segment(aes(x = 0.240, y = -0.150,
                   xend = 0.256, yend = 0.232),
               arrow = arrow(),
               color = "red",
               linewidth = 0.5) +
  annotate("text",
           x = 0.24, y = -0.2,
           label = "2025 Wk1 vs. DAL",
           color = "red",
           fontface = "italic",
           size = 4.8) +
  labs(title = "J. Hurts Scramble Rate vs. EPA/Dropback by Game",
       subtitle = "*min. 20 dropbacks",
       x = "Scramble Rate",
       y = "EPA/Dropback",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey25"),
        panel.grid.minor.y = element_line(color = "grey25"),
        panel.grid.major.x = element_line(color = "grey25"),
        panel.grid.minor.x = element_line(color = "grey25"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#1A1A1A"),
        plot.title = element_text(face = "bold",
                                  size = 22,
                                  color ="#CCCCCC"),
        plot.subtitle = element_text(face = "bold.italic",
                                     size = 18,
                                     color = "#CCCCCC"),
        plot.caption = element_text(size = 13,
                                    color = "#CCCCCC"),
        axis.title.y = element_text(face = "bold",
                                    size = 16,
                                    color = "#CCCCCC"),
        axis.text.y = element_text(size = 14,
                                   color = "#CCCCCC"),
        axis.title.x = element_text(face = "bold",
                                    size = 16,
                                    color = "#CCCCCC"),
        axis.text.x = element_text(size = 14,
                                   color = "#CCCCCC"))

# view plot
hurtsplot1

# save plot to local files
ggsave("X post 62 - hurts_scrambles.png",
       width = 10.5, height = 7,
       dpi = "retina")
