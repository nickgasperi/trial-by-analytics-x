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
# include only regular season plays with a targeted receiver
wrdata6 = nfldata %>%
  filter(week < 19,
         !is.na(yards_gained),
         !is.na(receiver_player_id)) %>%
  group_by(play_id,
           receiver_player_id,
           receiver_player_name,
           posteam) %>%
  summarize(tgts = n(),
            yds = sum(yards_gained),
            .groups = "drop")

# add cumulative rec yds and targets columns to existing tibble
wrdata6$cumyds = ave(wrdata6$yds, wrdata6$receiver_player_id, FUN = cumsum)
wrdata6$cumtgts = ave(wrdata6$tgts, wrdata6$receiver_player_id, FUN = cumsum)

# frame last targets of dataset for later geom_point()
framewr6 = wrdata6 %>%
  filter(play_id == "5028") %>%
  print(n = Inf)

# plot data
wrplot6 = ggplot(data = wrdata6,
                 aes(x = play_id, y = cumyds,
                     group = receiver_player_id)) +
  geom_line(aes(color = "#FF5C00"),
            linewidth = 1.0) +
  gghighlight(receiver_player_id == "00-0036900",
              label_key = receiver_player_name,
              use_direct_label = FALSE,
              unhighlighted_params = list(linewidth = 0.5)) +
  geom_point(data = framewr6,
             aes(color = "#FF5C00"),
             size = 2.5) +
  geom_text_repel(data = framewr6,
                  box.padding = 0.3,
                  aes(label = cumyds,
                      color = "#FF5C00",
                      family = "Tw Cen MT Condensed Extra Bold"),
                  size = 8.0) +
  scale_color_identity() +
  scale_y_continuous(breaks = c(0, 400, 800, 1200, 1600)) +
  coord_cartesian(clip = "off") +
  annotation_custom(cinLogo,
                    x = 4950, y = 1795,
                    xmax = 5600, ymax = 2000) +
  labs(title = "Ja'Marr Chase Triple Crown - Receiving Yards",
       subtitle = "2024 NFL Regular Season",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       y = "Receiving Yards") +
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
wrplot6

# save plot to local files
ggsave("X Post 56 - chase_TC_yards.png",
       width = 10.5, height = 7,
       dpi = "retina")
