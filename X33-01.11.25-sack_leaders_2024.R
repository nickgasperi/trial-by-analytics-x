# load packages
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(nflplotR)

# load 2024 NFL data
data3 = load_pbp(2024)

# filter data
sackdata = data3 %>%
  filter(!is.na(sack),
         !is.na(sack_player_id)) %>%
  group_by(sack_player_id,
           sack_player_name,
           defteam) %>%
  summarize(sacks = sum(sack)) %>%
  arrange(-sacks) %>%
  filter(sacks >= 12) %>%
  print(n = Inf)

# plot data
sackplot2 = ggplot(data = sackdata,
                   aes(x = reorder(sack_player_id, -sacks), y = sacks)) +
  geom_col(aes(fill = defteam,
               color = defteam),
           width = 0.8) +
  geom_text(label = round(sackdata$sacks, 2),
            position = position_stack(vjust = 0.5),
            fontface = "bold.italic",
            color = "white",
            size = 9) +
  geom_nfl_logos(aes(team_abbr = defteam),
                 width = 0.07) +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl() +
  labs(title = "2024 NFL Reg. Season Sack Leaders",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       y = "Sacks") +
  theme_minimal() +
  theme(plot.background = element_rect(fill= "#F0F0F0"),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 20),
        plot.caption = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.text.x = element_nfl_headshot(size = 2.8),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

# view plot
sackplot2

# save plot to local files
ggsave("X post 33 - sack_leaders_2024.png",
       width = 10.5, height = 7,
       dpi = "retina")  
