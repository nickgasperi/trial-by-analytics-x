# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(gghighlight)
library(ggrepel)
library(ggimage)
library(ggdark)
library(png)
library(grid)

# load bengals logo image
image = "C:/Users/Nick Gasperi/Downloads/cin-logo-two.png"

# download image to R
cinLogo = readPNG(image) %>%
  rasterGrob(interpolate = TRUE)

# load data
nfldata = load_pbp(2024)

# filter data
wrdata7 = nfldata %>%
  filter(week < 19,
         !is.na(receiver_player_id)) %>%
  group_by(play_id, receiver_player_id, receiver_player_name, posteam) %>%
  summarize(tgt = n(),
            rec = complete_pass,
            .groups = "drop")

# create cumulative rec yds and targets columns
wrdata7$cumtgt = ave(wrdata7$tgt, wrdata7$receiver_player_id, FUN = cumsum)
wrdata7$cumrec = ave(wrdata7$rec, wrdata7$receiver_player_id, FUN = cumsum)

# frame last play for later future labeling
framewr7 = wrdata7 %>%
  filter(play_id == "5028") %>%
  print(n = Inf)

# plot data
# use gghighlight to emphasize J.Chase data point
# use 'use_direct_label' to allow you to create your own label with 'geom_point' & geom_text_repel'
wrplot7 = ggplot(data = wrdata7, aes(x = play_id, y = cumrec, group = receiver_player_id)) +
  geom_line(aes(color = "#FF5C00"),
            linewidth = 1.0) +
  gghighlight(receiver_player_id == "00-0036900",
              label_key = receiver_player_name,
              use_direct_label = FALSE,
              unhighlighted_params = list(linewidth = 0.5)) +
  geom_point(data = framewr7,
             aes(color = "#FF5C00"),
             size = 2.5) +
  geom_text_repel(box.padding = 0.4,
                  data = framewr7,
                  aes(label = cumrec,
                      color = "#FF5C00",
                      family = "Tw Cen MT Condensed Extra Bold"),
                  segment.color = NA,
                  size = 8.5) +
  scale_y_continuous(breaks = c(0, 30, 60, 90, 120)) +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  annotation_custom(cinLogo,
                    x = 4920, y = 134,
                    xmax = 5650, ymax = 149) +
  labs(title = "Ja'Marr Chase Triple Crown - Receptions",
       subtitle = "2024 NFL Regular Season",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       y = "Receptions") +
  dark_theme_gray() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey25"),
        panel.grid.minor.y = element_line(color = "grey25"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 23,
                                  color = "white"),
        plot.subtitle = element_text(size = 21,
                                     color = "white"),
        plot.caption = element_text(size = 13,
                                    color = "white"),
        axis.title.y = element_text(size = 17,
                                    color = "white"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 17,
                                   color = "white"),
        axis.ticks = element_blank()) +
  theme(text = element_text(family = "Tw Cen MT Condensed Extra Bold"))

# view plot
wrplot7
  
# save plot
ggsave("X Post 57 - chase_TC_receptions.png",
       width = 10.5, height = 7, dpi = "retina")
