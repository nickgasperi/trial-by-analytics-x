# load packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(nflreadr)

# load data from '18-'24 NFL seasons
nfldata2 = load_pbp(2018:2024)

# filter data to Barkley reg season rushing total by year
# rename columns to 'id' and 'name' for future joining of tibbles
saqrush = nfldata2 %>%
  filter(season_type == "REG",
         rusher_player_id == "00-0034844",
         !is.na(yards_gained),
         !is.na(epa)) %>%
  group_by(season, rusher_player_id,
           rusher_player_name,
           posteam) %>%
  summarize(rushyd = sum(yards_gained)) %>%
  rename(id = rusher_player_id,
         name = rusher_player_name) %>%
  print(n = Inf)

# filter data to Barkley reg season receiving total by year
# rename columns to 'id' and 'name' for future joining of tibbles
saqrec = nfldata2 %>%
  filter(season_type == "REG",
         receiver_player_id == "00-0034844",
         !is.na(yards_gained)) %>%
  group_by(season,
           receiver_player_id,
           receiver_player_name,
           posteam) %>%
  summarize(recyd = sum(yards_gained)) %>%
  rename(id = receiver_player_id,
         name = receiver_player_name) %>%
  print(n = Inf)

# join the two tibbles
# include all common/shared values in the join code, otherwise it will repeat them in duplicate columns
# calculate total yards 
saqyds = saqrush %>%
  left_join(saqrec, by = c("season", "id", "name")) %>%
  summarize(rushyd = rushyd,
            recyd = recyd,
            totyd = sum(rushyd) + sum(recyd)) %>%
  print(n = Inf)

# manually add team identifier columns
saqyds$posteam = c("NYG", "NYG", "NYG", "NYG", "NYG", "NYG", "PHI")

# view updated tibble
saqyds

# plot saqyds tibble
saqplot1 = ggplot(data = saqyds,
                  aes(x = season, y = totyd)) +
  geom_col(aes(fill = posteam,
               color = posteam),
           linewidth = 1.5) +
  scale_fill_nfl(type = "secondary") +
  scale_color_nfl(type = "primary") +
  geom_nfl_logos(aes(team_abbr = posteam,
                     width = 0.08)) +
  scale_x_continuous(n.breaks = 7) +
  labs(title = "Saquon Barkley Scrimmage Yards By Year",
       caption = "By Nick Gasperi | @tbanalysis | Data @nflfastR",
       x = "Season",
       y = "Total Yards") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "cornsilk"),
        plot.title = element_text(hjust = 0.5, face = "bold.italic",
                                  color = "grey30",
                                  size = 18),
        plot.caption = element_text(size = 11),
        axis.title = element_text(face = "bold",
                                  size = 15),
        axis.text = element_text(face = "bold.italic",
                                 size = 15))

# view plot
saqplot1

# save plot to local files
ggsave("X post 47.1 - saquon_yards.png",
       width = 10.5, height = 7,
       dpi = "retina")
