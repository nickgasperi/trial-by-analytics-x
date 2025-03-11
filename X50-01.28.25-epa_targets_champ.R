# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)
library(ggrepel)

# load NFL data from 2024 season
nfldata = load_pbp(2024)

# wrangle data into new tibble
champdatarec = nfldata %>%
  filter(week == 21,
         !is.na(epa),
         !is.na(air_yards),
         !is.na(receiver_player_id)) %>%
  group_by(receiver_player_id, receiver_player_name, posteam) %>%
  summarize(targets = n(),
            epaper = sum(epa)/sum(targets),
            recyd = sum(yards_gained)) %>%
  arrange(-targets) %>%
  filter(targets >= 3) %>%
  print(n = Inf)

# plot data
champplotrec = ggplot(data = champdatarec, aes(x = targets, epaper)) +
  geom_hline(yintercept = mean(champdatarec$epaper),
             linetype = "dashed", color = "grey30") +
  geom_hline(yintercept = 0,
             color = "chartreuse", alpha = 0.7) +
  geom_vline(xintercept = mean(champdatarec$targets),
             linetype = "dashed", color = "grey30") +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_nfl_logos(aes(team_abbr = posteam),
                 width = 0.06, alpha = 0.8) +
  geom_text_repel(box.padding = 0.8,
                  aes(label = receiver_player_name,
                      color = posteam, fontface = "bold")) +
  scale_color_nfl(type = "primary") +
  labs(title = "Volume vs. Efficiency of Targets",
       subtitle = "2024 AFC/NFC Championships | min. 3 targets",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Targets", y = "EPA/Target") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5,
                                     face = "bold", size = 16),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(size = 15))
      
# view plot
champplotrec

# save plot to device's local files
ggsave("X post 50 - epa_targets_champ_wknd.png",
       width = 10.5, height = 7, dpi = "retina")
