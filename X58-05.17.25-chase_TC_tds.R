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
# include only regular season passing plays
wrdata8 = nfldata %>%
  filter(week < 19,
         play_type == "pass",
         !is.na(receiver_player_id)) %>%
  group_by(week,
           receiver_player_id,
           receiver_player_name,
           posteam) %>%
  summarize(tgt = n(),
            td = sum(touchdown == 1 &
                       td_team == posteam &
                       td_player_id == receiver_player_id),
            .groups = "drop")

# create cumulative rec tds and targets columns
wrdata8$cumtgt = ave(wrdata8$tgt, wrdata8$receiver_player_id, FUN = cumsum)
wrdata8$cumtd = ave(wrdata8$td, wrdata8$receiver_player_id, FUN = cumsum)

# frame last week for later future labeling
framewr8 = wrdata8 %>%
  filter(week == 18,
         receiver_player_id == "00-0036900") %>%
  print(n = Inf)

# plot data
# use gghighlight() to emphasize J.Chase data point
wrplot8 = ggplot(data = wrdata8,
                 aes(x = week, y = cumtd,
                     group = receiver_player_id)) +
  geom_line(aes(color = "#FF5C00"),
            linewidth = 1.0) +
  gghighlight(receiver_player_id == "00-0036900",
              label_key = receiver_player_name,
              use_direct_label = FALSE,
              unhighlighted_params = list(linewidth = 0.5)) +
  geom_point(data = framewr8,
             aes(color = "#FF5C00"),
             size = 2.5) +
  geom_text_repel(box.padding = 0.9,
                  data = framewr8,
                  aes(label = cumtd,
                      color = "#FF5C00",
                      family = "Tw Cen MT Condensed Extra Bold"),
                  segment.color = NA,
                  size = 9.0) +
  scale_color_identity() +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16)) +
  coord_cartesian(clip = "off") +
  annotation_custom(cinLogo,
                    x = 16.73, y = 18.03,
                    xmax = 19.75, ymax = 19.97) +
  labs(title = "Ja'Marr Chase Triple Crown - Receiving TDs",
       subtitle = "2024 NFL Regular Season",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Week",
       y = "Receiving Touchdowns") +
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
wrplot8

# save plot
ggsave("X Post 58 - chase_TC_tds.png",
       width = 10.5, height = 7,
       dpi = "retina")
