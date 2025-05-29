# load packages
library(tidyverse)
library(ggrepel)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load NFL data from 2024 season
data3 = load_pbp(2024)

# filter data
wrdata1 = data3 %>%
  filter(receiver_player_id == "00-0035659" | receiver_player_id == "00-0039893",
         !is.na(yards_gained),
         !is.na(epa)) %>%
  group_by(play_id,
           receiver_player_id,
           receiver_player_name,
           posteam) %>%
  summarize(targets = n(),
            rec = sum(complete_pass),
            yards = sum(yards_gained),
            epa = sum(epa),
            epapert = sum(epa)/sum(targets),
            .groups = "drop")

# view bottom of new tibble
tail(wrdata1, 10)

# add columns for cumulative yards, epa, and targets
wrdata1$cumyds = ave(wrdata1$yards, wrdata1$receiver_player_id, FUN = cumsum)
wrdata1$cumepa = ave(wrdata1$epa, wrdata1$receiver_player_id, FUN = cumsum)
wrdata1$cumtgts = ave(wrdata1$targets, wrdata1$receiver_player_id, FUN = cumsum)

# frame last targets for later geom_point
frame3 = wrdata1 %>%
  filter(play_id == "4873" | play_id == "4726") %>%
  print(n = Inf)

# create plot
plotwr1 = ggplot(data = wrdata1,
                 aes(x = cumtgts, y = cumepa)) +
  geom_line(aes(color = posteam)) +
  geom_point(data = frame3,
             aes(color = posteam)) +
  geom_text_repel(data = frame3,
                  aes(label = receiver_player_name,
                      fontface = "bold.italic",
                      color = posteam,
                      size = 3)) +
  scale_color_nfl(type = "primary") +
  labs(title = "Terry McLaurin vs. Brian Thomas Jr. - Cumulative EPA",
       subtitle = "2024 NFL Reg Season | Passing Plays",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Targets",
       y = "EPA") +
  scale_x_continuous(breaks = seq(0, 140, 30)) +
  scale_y_continuous(breaks = seq(-10, 75, 20)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 19),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold",
                                     size = 17),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold.italic",
                                  size = 15),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14))

# view plot
plotwr1

# save plot to local files
ggsave("X post 30 - wr_comp_epa.png",
       width = 10.5, height = 7,
       dpi = "retina")
