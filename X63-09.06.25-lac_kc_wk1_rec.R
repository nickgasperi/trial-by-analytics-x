# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(ggrepel)
library(png)
library(grid)

# load logo images
chiefs_logo = "C:/Users/Nick Gasperi/Downloads/old_chiefs_logo.png"
chargers_logo = "C:/Users/Nick Gasperi/Downloads/old_chargers_logo.png"

# download images in R
chiefs_logo_img = readPNG(chiefs_logo) %>%
  rasterGrob(interpolate = TRUE)

chargers_logo_img = readPNG(chargers_logo) %>%
  rasterGrob(interpolate = TRUE)

# load nfl play-by-play data
nfldata = load_pbp(2025)

# create new tibble for per target stats
targets1 = nfldata %>%
  mutate(yards_after_catch = ifelse(is.na(yards_after_catch), 0, yards_after_catch)) %>%
  filter(game_id == "2025_01_KC_LAC",
         !is.na(receiver_player_id)) %>%
  group_by(receiver_player_id,
           receiver_player_name,
           posteam) %>%
  summarize(targets = n(),
            rec_yds = sum(yards_gained),
            yac = sum(yards_after_catch),
            epa = sum(epa),
            epa_per_t = epa/targets) %>%
  arrange(-targets) %>%
  filter(targets >= 4) %>%
  print(n = Inf)

# plot targets vs. epa/target
kc_lac_wr_plot_a = ggplot(data = targets1,
                          aes(x = targets, y = epa_per_t)) +
  geom_hline(yintercept = 0,
             color = "grey20",
             linetype = "dashed",
             alpha = 0.50) +
  geom_point(aes(color = posteam,
                 size = yac),
             stroke = 2) +
  geom_text_repel(box.padding = 0.65,
                  aes(label = receiver_player_name,
                      color = posteam),
                  size = 5,
                  segment.color = NA) +
  scale_color_nfl(type = "primary") +
  coord_cartesian(clip = "off") +
  annotation_custom(chiefs_logo_img,
                    x = 15.2, y = 0.97,
                    xmax = 16.8, ymax = 1.13) +
  annotation_custom(chargers_logo_img,
                    x = 13.9, y = 0.97,
                    xmax = 15.5, ymax = 1.13) +
  labs(title = "LAC vs. KC - Targets vs. EPA/Target",
       subtitle = "2025 Wk1 | min. 4 targets | size = total YAC",
       x = "Targets",
       y = "EPA/Target",
       caption = "By Nick Gasperi | @tbanalysis | data @nflfastR | logos: sportslogos.net") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold",
                                  size = 22),
        plot.subtitle = element_text(face = "bold.italic",
                                     size = 16),
        plot.caption = element_text(size = 13),
        axis.title.y = element_text(face = "bold",
                                    size = 16),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(face = "bold",
                                    size = 16),
        axis.text.x = element_text(size = 14))

# view plot
kc_lac_wr_plot_a

# save plot to local files
ggsave("X post 63 - kc_lac_wk1_rec.png",
       width = 10.5, height = 7,
       dpi = "retina")

