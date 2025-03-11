# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggrepel)

# load data
nfldata = load_pbp(2024)

# wrangle data into new tibble
wk20data = nfldata %>%
  filter(week == 20,
         !is.na(epa),
         !is.na(receiver_player_id),
         !is.na(yards_after_catch)) %>%
  group_by(receiver_player_id, receiver_player_name, posteam) %>%
  summarize(rec = n(),
            epaper = sum(epa)/sum(rec),
            yacper = sum(yards_after_catch)/sum(rec)) %>%
  filter(rec > 5) %>%
  print(n = Inf)

# plot data
wk20rec1 = ggplot(data = wk20data, aes(x = yacper, y = epaper)) +
  geom_hline(yintercept = mean(wk20data$epaper), linetype = "dashed", color = "black") +
  geom_vline(xintercept = mean(wk20data$yacper), linetype = "dashed", color = "black") +
  geom_nfl_logos(aes(team_abbr = posteam), width = 0.065, alpha = 0.8) +
  geom_text_repel(box.padding = 0.8,
                  aes(label = wk20data$receiver_player_name,
                      color = posteam, fontface = "bold.italic", size = 2)) +
  scale_color_nfl(type = "primary") +
  labs(title = "YAC vs. EPA Per Reception",
       subtitle = "2024 NFL Divisional Round | min. 6 rec.",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "YAC", y = "EPA") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(size = 15))

# view plot
wk20rec1

# save plot to device's local files
ggsave("X post 45 - div_yac_epa.png",
       width = 10.5, height = 7, dpi = "retina")
